get_query <- function(url, query, skip = NULL, top = NULL) {
  final <- query

  if (!is.null(skip)) {
    final <- c(final, glue::glue("$skip={skip}"))
  }

  if (!is.null(top)) {
    final <- c(final, glue::glue("$top={top}"))
  }

  final <- paste(final, collapse = "&")
  final <- paste0("?", final)

  resp <- file.path(url, final) |>
    httr2::request() |>
    httr2::req_cache(path = tempdir()) |>
    httr2::req_retry(
      max_tries = 10,
      is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 503),
      backoff = ~ 10
    ) |>
    httr2::req_perform()

  resp <- resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  resp$value |>
    dplyr::as_tibble() |>
    dplyr::rename_with(tolower)
}

#' List AWWID database tables
#'
#' @return a character voect
#' @export
#'
#' @examples
#' list_awwid()
list_awwid <- function() {
  url <-
    "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"

  # some checks
  metadata <- url |>
    httr2::request() |>
    httr2::req_perform()

  metadata <- metadata |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  metadata$value$name[1:25]
}

#' Fetch AWWID data from the AEPA OData server
#'
#' @details
#' AWWID can can be obtained through standard REST requests to
#' <https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata>
#'
#' @param name name of the AWWID table.
#' @param filter filter query to select rows using REST expressions. The query
#'   can limit the data using 'eq' in place of 'equal-to', and 'le' in place of
#'   'less-than', and 'gt' in place of 'greater-than'.
#' @param select columns to select from the table. By default, all columns will
#'   be returned.
#' @param top Limit the results using the 'top' rows
#'
#' @return a tibble
#' @export
#'
#' @examples
#' request_awwid("wells", top = 10)
request_awwid <- function(name, filter = NULL, select = NULL, top = NULL) {
  url <-
    "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"

  # some checks
  metadata <- url |>
    httr2::request() |>
    httr2::req_retry(max_tries = 10) |>
    httr2::req_perform()

  metadata <- metadata |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  if (!tolower(name) %in% tolower(metadata$value$name)) {
    rlang::abort(glue::glue(
      "`name` must be one of {tables}",
      tables = paste(metadata$value$name, collapse = ", ")
    ))
  }

  # build request
  r <- file.path(url, name)

  query <- c()

  if (!is.null(filter)) {
    filter <- utils::URLencode(filter)
    query <- c(query, glue::glue("$filter={filter}"))
  }

  if (!is.null(select)) {
    select <- utils::URLencode(select)
    query <- c(query, glue::glue("$select={select}"))
  }

  # count number of records that request will generate
  query_count <- c(query, "$count=true")
  query_count <- paste(query_count, collapse = "&")
  query_count <- paste0("?", query_count)
  resp <- file.path(r, query_count) |>
    httr2::request() |>
    httr2::req_retry(max_tries = 10) |>
    httr2::req_perform()
  counts <- as.integer(httr2::resp_body_json(resp)[["@odata.count"]])

  if ((is.null(top) || top > 10000) & counts > 10000) {
    df <- furrr::future_map_dfr(
      .x = seq(0L, counts, by = 10000L),
      ~ get_query(url = r, query = query, skip = .x),
      .progress = TRUE
    )
  } else {
    df <- get_query(url = r, query = query, top = top)
  }

  class(df) <- c(tolower(name), "awwid", class(df))
  return(df)
}
