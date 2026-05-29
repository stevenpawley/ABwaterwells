#' Target factory for chunk-cached AWWID table download
#'
#' Creates four [targets::tar_target()] targets that download a table in
#' independently cached chunks. Only chunks whose offset falls within a changed
#' row-count range are re-downloaded on subsequent runs.
#'
#' @param name symbol, name for the assembled table target.
#' @param table character string naming the table, or a field from
#'   [awwid_connect()] (e.g. `con$wells`).
#' @param filter OData filter expression.
#' @param select character vector of columns to return.
#' @param chunk_size integer, rows per chunk.
#'
#' @return a list of [targets::tar_target()] objects.
#' @export
tar_awwid_table <- function(
  name,
  table,
  filter = NULL,
  select = NULL,
  chunk_size = 10000L
) {
  name_str <- deparse(substitute(name))
  count_name <- paste0(name_str, "_count")
  skips_name <- paste0(name_str, "_skips")
  chunk_name <- paste0(name_str, "_chunk")
  count_sym <- as.name(count_name)
  skips_sym <- as.name(skips_name)
  chunk_sym <- as.name(chunk_name)

  list(
    targets::tar_target_raw(
      count_name,
      bquote(awwid_count(.(table)))
    ),
    targets::tar_target_raw(
      skips_name,
      bquote(seq(0L, .(count_sym) - 1L, by = .(chunk_size)))
    ),
    targets::tar_target_raw(
      chunk_name,
      bquote(awwid_chunk(
        .(table),
        skip = .(skips_sym),
        top = .(chunk_size),
        filter = .(filter),
        select = .(select)
      )),
      pattern = bquote(map(.(skips_sym)))
    ),
    targets::tar_target_raw(
      name_str,
      bquote(dplyr::bind_rows(.(chunk_sym)))
    )
  )
}
