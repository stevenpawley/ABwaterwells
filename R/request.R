build_query_url <- function(base_url, query, skip = NULL, top = NULL) {
  if (!is.null(skip)) {
    query <- c(query, glue("$skip={skip}"))
  }
  if (!is.null(top)) {
    query <- c(query, glue("$top={top}"))
  }
  qs <- paste(query, collapse = "&")
  if (nzchar(qs)) {
    paste0(base_url, "?", qs)
  } else {
    base_url
  }
}

build_request <- function(url) {
  httr2::request(url) |>
    httr2::req_cache(path = tempdir()) |>
    httr2::req_retry(
      max_tries = 15,
      is_transient = ~ httr2::resp_status(.x) %in% c(403, 429, 500, 502, 503),
      backoff = \(x) min(10 * 2^(x - 1), 120)
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


#' Connect to the AEPA AWWID OData service
#'
#' @description
#' Fetches the list of available tables and returns a connection object. Each
#' table is accessible as a named field (e.g. `con$wells`, `con$wellreports`),
#' which can be passed directly to [awwid_tbl()], [awwid_count()], or
#' [awwid_chunk()] — giving tab-completion without typing table names as strings.
#'
#' @return an `awwid_connection` object.
#' @export
#'
#' @examples
#' con <- awwid_connect()
#' con
#'
#' awwid_tbl(con$wells, top = 10)
#' awwid_count(con$wellreports)
awwid_connect <- function() {
  tables <- tolower(awwid_list_tables())
  structure(
    stats::setNames(as.list(tables), tables),
    class = "awwid_connection"
  )
}

#' @export
print.awwid_connection <- function(x, ...) {
  tables <- names(x)
  cat(sprintf("<awwid_connection> [%d tables]\n", length(tables)))
  lines <- strwrap(paste(tables, collapse = ", "), width = 72, prefix = "  ")
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' Get the row count for an AWWID table
#'
#' @description
#' Makes a single lightweight request (`$count=true&$top=0`) and returns the
#' total row count for a table. Useful as a [targets] sentinel: because
#' `targets` re-runs a target only when its dependencies change, a count target
#' will invalidate the full download target only when the server data changes.
#'
#' @param table character string naming the table, or a field from
#'   [awwid_connect()] (e.g. `con$wells`).
#'
#' @return an integer row count.
#' @export
#'
#' @examples
#' con <- awwid_connect()
#' awwid_count(con$wells)
awwid_count <- function(table) {
  url <- "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"
  count_url <- paste0(url, "/", tolower(table), "?$count=true&$top=0")

  resp <- httr2::request(count_url) |>
    httr2::req_retry(
      max_tries = 15,
      is_transient = ~ httr2::resp_status(.x) %in% c(403, 429, 500, 502, 503),
      backoff = \(x) min(10 * 2^(x - 1), 120)
    ) |>
    httr2::req_perform()

  count_val <- httr2::resp_body_json(resp)[["@odata.count"]]
  if (is.null(count_val)) {
    abort(
      "Server did not return @odata.count; verify the OData service supports $count."
    )
  }
  as.integer(count_val)
}

#' Download a single chunk of an AWWID table
#'
#' @description
#' Downloads one page of rows using explicit `skip` and `top` offsets. Intended
#' for use with [targets] dynamic branching, where each chunk is an independent
#' cached target that is only re-downloaded when the upstream count changes.
#'
#' @param table character string naming the table, or a field from
#'   [awwid_connect()] (e.g. `con$wells`).
#' @param skip integer, number of rows to skip (chunk offset).
#' @param top integer, number of rows to return.
#' @param filter OData filter expression to select rows server-side.
#' @param select character vector of columns to return.
#'
#' @return a classed tibble (same class as [awwid_tbl()]).
#' @export
#'
#' @examples
#' con <- awwid_connect()
#' awwid_chunk(con$wells, skip = 0L,     top = 10000L)
#' awwid_chunk(con$wells, skip = 10000L, top = 10000L)
awwid_chunk <- function(
  table,
  skip = 0L,
  top = 10000L,
  filter = NULL,
  select = NULL
) {
  url <- "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"
  name <- tolower(table)
  r <- paste(url, name, sep = "/")

  query <- c()
  if (!is.null(filter)) {
    query <- c(query, glue("$filter={utils::URLencode(filter)}"))
  }
  if (!is.null(select)) {
    if (length(select) > 1) {
      select <- paste(sort(select), collapse = ",")
    }
    query <- c(query, glue("$select={utils::URLencode(select)}"))
  }

  df <- get_query(r, query, skip = skip, top = top)
  class(df) <- c(name, "awwid", class(df))
  df
}

#' Fetch AWWID data from the AEPA OData server
#'
#' @description
#' Downloads a table from the AWWID OData service. Pass a field from
#' [awwid_connect()] (e.g. `con$wells`) for tab-completion, or a plain
#' character string for programmatic use.
#'
#' @details
#' Tables larger than `chunk_size` rows are downloaded in parallel chunks using
#' a deterministic `$orderby` on the primary key to prevent gaps or duplicates
#' at chunk boundaries.
#'
#' @param table character string naming the table, or a field from
#'   [awwid_connect()] (e.g. `con$wells`).
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
awwid_tbl <- function(
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
    abort(glue(
      "`table` must be one of {tables}",
      tables = paste(names(metadata), collapse = ", ")
    ))
  }

  # validate column names
  if (!is.null(select)) {
    expected_columns <- names(metadata[[name]]$columns)
    incorrect_cols <- select[!select %in% expected_columns]
    if (length(incorrect_cols) > 0) {
      abort(glue(
        "The column(s) {cols} are not present in `{name}`",
        cols = paste(glue("'{incorrect_cols}'"), collapse = ", ")
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
    query <- c(query, glue("$filter={utils::URLencode(filter)}"))
  }
  if (!is.null(select)) {
    query <- c(query, glue("$select={utils::URLencode(select)}"))
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
      backoff = \(x) min(10 * 2^(x - 1), 120)
    ) |>
    httr2::req_perform()

  count_val <- httr2::resp_body_json(resp)[["@odata.count"]]
  if (is.null(count_val)) {
    abort(
      "Server did not return @odata.count; verify that the OData service supports $count."
    )
  }
  counts <- as.integer(count_val)

  # download in parallel chunks, ordered by primary key for deterministic pagination
  if ((is.null(top) || top > chunk_size) && counts > chunk_size) {
    pk_resp <- build_request(build_query_url(r, query, top = 1L)) |>
      httr2::req_perform(verbosity = 0)
    pk_col <- names(jsonlite::fromJSON(httr2::resp_body_string(pk_resp))$value)[
      1
    ]

    reqs <- lapply(seq(0L, counts - 1L, by = chunk_size), function(skip) {
      build_request(build_query_url(
        r,
        c(query, glue("$orderby={pk_col}")),
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
      rlang::abort(
        glue(
          "{sum(errors)} chunk(s) failed to download; try increasing retry attempts."
        )
      )
    }
    df <- dplyr::bind_rows(lapply(resps, parse_odata_response))
  } else {
    df <- get_query(r, query, top = top)
  }

  class(df) <- c(name, "awwid", class(df))
  return(df)
}
