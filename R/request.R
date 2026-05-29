# internal helpers ----

build_query_url <- function(base_url, query, skip = NULL, top = NULL) {
  if (!is.null(skip)) {
    query <- c(query, glue::glue("$skip={skip}"))
  }
  if (!is.null(top)) {
    query <- c(query, glue::glue("$top={top}"))
  }
  qs <- paste(query, collapse = "&")
  if (nzchar(qs)) paste0(base_url, "?", qs) else base_url
}

build_request <- function(url) {
  httr2::request(url) |>
    httr2::req_cache(path = tempdir()) |>
    httr2::req_retry(
      max_tries = 15,
      is_transient = ~ httr2::resp_status(.x) %in% c(403, 429, 500, 502, 503),
      backoff = \(x) 10
    )
}

parse_odata_response <- function(resp) {
  result <- httr2::resp_body_string(resp) |> jsonlite::fromJSON()
  dplyr::as_tibble(result$value) |> dplyr::rename_with(tolower)
}

get_query <- function(base_url, query, skip = NULL, top = NULL) {
  build_query_url(base_url, query, skip = skip, top = top) |>
    build_request() |>
    httr2::req_perform() |>
    parse_odata_response()
}

# exported functions ----

#' List AWWID database tables
#'
#' @return a character vector of table names available on the OData service.
#' @export
awwid_list_tables <- function() {
  url <- "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"

  metadata <- url |>
    httr2::request() |>
    httr2::req_perform()

  metadata <- metadata |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  metadata$value$name
}

#' Connect to the AEPA AWWID OData service
#'
#' @description
#' Fetches the list of available tables and returns a connection object whose
#' named fields are `awwid_table` descriptors — one per table. Pass any field
#' directly to [awwid_tbl()] instead of typing a table name as a string.
#'
#' @return an `awwid_connection` object.
#' @export
#'
#' @examples
#' con <- awwid_connect()
#' con
#'
#' awwid_tbl(con$wells, top = 10)
#' awwid_tbl(con$wellreports, select = c("wellid", "totaldepthdrilled"))
awwid_connect <- function() {
  tables <- tolower(awwid_list_tables())
  descriptors <- setNames(
    lapply(tables, \(t) structure(list(name = t), class = "awwid_table")),
    tables
  )
  structure(descriptors, class = "awwid_connection")
}

#' @export
print.awwid_connection <- function(x, ...) {
  tables <- names(x)
  cat(sprintf("<awwid_connection> [%d tables]\n", length(tables)))
  lines <- strwrap(paste(tables, collapse = ", "), width = 72, prefix = "  ")
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' @export
print.awwid_table <- function(x, ...) {
  cat(sprintf("<awwid_table: %s>\n", x$name))
  invisible(x)
}

#' Fetch AWWID data from the AEPA OData server
#'
#' @description
#' Downloads a table from the AWWID OData service. The `table` argument accepts
#' either a character string or an `awwid_table` descriptor from
#' [awwid_connect()], which avoids typing table names as strings and enables
#' tab completion.
#'
#' @details
#' Tables larger than `chunk_size` rows are downloaded in parallel chunks using
#' a deterministic `$orderby` on the primary key to prevent gaps or duplicates
#' at chunk boundaries.
#'
#' @param table an `awwid_table` descriptor from [awwid_connect()], or a
#'   character string naming the table.
#' @param filter OData filter expression to select rows server-side.
#' @param select character vector of columns to return. By default all columns
#'   are returned.
#' @param top integer, return only the first n rows.
#' @param chunk_size integer, number of rows per download chunk when paginating.
#'   Reducing this value forces chunking on small tables, which is useful for
#'   testing.
#' @param .progress logical, show a progress bar when downloading chunks.
#'
#' @return a tibble
#' @export
awwid_tbl <- function(table, ...) UseMethod("awwid_tbl")

#' @export
#' @rdname awwid_tbl
awwid_tbl.awwid_table <- function(
  table,
  filter = NULL,
  select = NULL,
  top = NULL,
  chunk_size = 10000L,
  .progress = FALSE
) {
  awwid_tbl.character(
    table$name,
    filter = filter,
    select = select,
    top = top,
    chunk_size = chunk_size,
    .progress = .progress
  )
}

#' @export
#' @rdname awwid_tbl
awwid_tbl.character <- function(
  table,
  filter = NULL,
  select = NULL,
  top = NULL,
  chunk_size = 10000L,
  .progress = FALSE
) {
  url <- "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"
  name <- tolower(table)

  names(metadata) <- tolower(names(metadata))

  # validate table name
  if (!name %in% names(metadata)) {
    rlang::abort(glue::glue(
      "`table` must be one of {tables}",
      tables = paste(names(metadata), collapse = ", ")
    ))
  }

  # validate column names
  if (!is.null(select)) {
    expected_columns <- names(metadata[[name]]$columns)
    incorrect_cols <- select[!select %in% expected_columns]
    if (length(incorrect_cols) > 0) {
      rlang::abort(glue::glue(
        "The column(s) {cols} are not present in `{name}`",
        cols = paste(glue::glue("'{incorrect_cols}'"), collapse = ", ")
      ))
    }
  }

  if (length(select) > 1) {
    select <- sort(select)
    select <- paste(select, collapse = ",")
  }

  # build base URL and query components
  r <- paste(url, name, sep = "/")
  query <- c()

  if (!is.null(filter)) {
    query <- c(query, glue::glue("$filter={utils::URLencode(filter)}"))
  }
  if (!is.null(select)) {
    query <- c(query, glue::glue("$select={utils::URLencode(select)}"))
  }

  # count records ($top=0 avoids transferring rows)
  count_url <- paste0(
    r,
    "?",
    paste(c(query, "$count=true", "$top=0"), collapse = "&")
  )
  resp <- httr2::request(count_url) |>
    httr2::req_retry(
      max_tries = 15,
      is_transient = ~ httr2::resp_status(.x) %in% c(403, 429, 500, 502, 503),
      backoff = \(x) 10
    ) |>
    httr2::req_perform()

  count_val <- httr2::resp_body_json(resp)[["@odata.count"]]
  if (is.null(count_val)) {
    rlang::abort(
      "Server did not return @odata.count; verify that the OData service supports $count."
    )
  }
  counts <- as.integer(count_val)

  # download in parallel chunks, ordered by primary key for deterministic pagination
  if ((is.null(top) || top > chunk_size) && counts > chunk_size) {
    # discover primary key column name (original casing) for $orderby
    pk_resp <- build_request(build_query_url(r, query, top = 1L)) |>
      httr2::req_perform(verbosity = 0)
    pk_col <- names(jsonlite::fromJSON(httr2::resp_body_string(pk_resp))$value)[
      1
    ]

    reqs <- lapply(seq(0L, counts - 1L, by = chunk_size), function(skip) {
      build_request(build_query_url(
        r,
        c(query, glue::glue("$orderby={pk_col}")),
        skip = skip,
        top = chunk_size
      ))
    })

    resps <- httr2::req_perform_parallel(
      reqs,
      on_error = "continue",
      progress = .progress
    )
    errors <- !vapply(resps, inherits, logical(1), "httr2_response")
    if (any(errors)) {
      rlang::abort(glue::glue(
        "{sum(errors)} chunk(s) failed to download; try increasing retry attempts."
      ))
    }
    df <- dplyr::bind_rows(lapply(resps, parse_odata_response))
  } else {
    df <- get_query(r, query, top = top)
  }

  class(df) <- c(name, "awwid", class(df))
  return(df)
}
