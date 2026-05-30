# Resolve an awwid tibble or character string to an entity name matching a nav
# list key (PascalCase singular, e.g. "WellReport").
awwid_entity_name <- function(x, nav) {
  if (is.character(x)) {
    m <- names(nav)[tolower(names(nav)) == tolower(x)]
    if (length(m) == 1L) return(m)
    abort(glue("Entity '{x}' not found in nav."))
  }
  if (!inherits(x, "awwid")) {
    abort("`x` must be an awwid tibble (from awwid_tbl()) or a character entity name.")
  }
  # The first class element is the lowercase plural table name set by awwid_tbl()
  tbl <- class(x)[1L]
  nav_lower <- tolower(names(nav))

  # Try progressively looser singularisation rules
  candidates <- c(
    tbl,
    sub("ies$", "y", tbl),   # companies -> company
    sub("ses$", "s", tbl),   # statuses  -> status
    sub("s$",   "",  tbl)    # wells     -> well
  )
  for (cand in candidates) {
    m <- names(nav)[nav_lower == cand]
    if (length(m) == 1L) return(m)
  }
  abort(glue(
    "Cannot resolve entity name for awwid table '{tbl}'. ",
    "Pass the entity name as a character string instead."
  ))
}

#' Auto-join two AWWID tables using service-defined relationships
#'
#' @description
#' Looks up the foreign-key relationship between two tables in a navigation
#' property list returned by [awwid_metadata_xml()] and performs the
#' appropriate dplyr join — no need to remember column names.
#'
#' Both `x` and `y` can be awwid tibbles returned by [awwid_tbl()] (the table
#' name is inferred automatically from their class) or plain character strings
#' matching an entity name in `nav` (PascalCase singular, e.g. `"WellReport"`).
#'
#' @param x,y Data frames to join, or character entity names.
#' @param nav Navigation property list from [awwid_metadata_xml()]. Pass a
#'   pre-fetched list to avoid repeated HTTP requests.
#' @param type Join type: `"left"` (default), `"inner"`, `"right"`, or
#'   `"full"`.
#'
#' @return A tibble: `x` joined to `y` on the columns defined by the service
#'   metadata.
#' @export
#'
#' @examples
#' nav <- awwid_metadata_xml()
#' con <- awwid_connect()
#'
#' wells <- awwid_tbl(con$wells, top = 100L)
#' drilling_companies <- awwid_tbl(con$drillingcompanies, top = 100L)
#'
#' # Table names inferred automatically from the awwid class
#' awwid_join(wells, drilling_companies, nav)
awwid_join <- function(x, y, nav, type = "left") {
  x_entity <- awwid_entity_name(x, nav)
  y_entity <- awwid_entity_name(y, nav)

  # Search x -> y (FK on x)
  xy <- nav[[x_entity]]
  xy <- xy[!is.na(xy$local_property) & xy$related_entity == y_entity, , drop = FALSE]

  # Search y -> x (FK on y)
  yx <- nav[[y_entity]]
  yx <- yx[!is.na(yx$local_property) & yx$related_entity == x_entity, , drop = FALSE]

  if (nrow(xy) > 0) {
    # x.local_property = y.referenced_property
    by <- stats::setNames(tolower(xy$referenced_property), tolower(xy$local_property))
  } else if (nrow(yx) > 0) {
    # y.local_property = x.referenced_property  (flip for dplyr: names=x, values=y)
    by <- stats::setNames(tolower(yx$local_property), tolower(yx$referenced_property))
  } else {
    abort(glue(
      "No navigation property found between '{x_entity}' and '{y_entity}'."
    ))
  }

  join_fn <- switch(type,
                    left  = dplyr::left_join,
                    right = dplyr::right_join,
                    inner = dplyr::inner_join,
                    full  = dplyr::full_join,
                    abort("`type` must be one of 'left', 'right', 'inner', or 'full'.")
  )

  join_fn(x, y, by = by)
}

