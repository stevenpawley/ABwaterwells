#' Fetch navigation properties from the AWWID OData service metadata
#'
#' @description
#' Retrieves the EDMX XML document from the OData `$metadata` endpoint and
#' returns the navigation properties declared for each entity type.  Navigation
#' properties describe foreign-key relationships between tables as defined by
#' the service itself.
#'
#' The result is intended to be fetched once and reused — pass it to
#' [awwid_join()] to perform automatic joins without specifying column names
#' manually.
#'
#' @return A named list with one element per entity type (names are PascalCase
#'   singular, e.g. `"Well"`, `"WellReport"`).  Each element is a data frame
#'   with one row per navigation property and the following columns:
#'   \describe{
#'     \item{`name`}{The navigation property label assigned by the service
#'       (e.g. `"Boreholes"`).  Usually matches `related_entity` but may
#'       differ for collection-side properties.}
#'     \item{`related_entity`}{The entity type on the other end of the
#'       relationship (e.g. `"Borehole"`).  Use this to identify which table
#'       is being referenced.}
#'     \item{`local_property`}{The foreign-key column on the current entity,
#'       or `NA` for collection-side (one-to-many) navigation properties where
#'       the FK lives on the related table.}
#'     \item{`referenced_property`}{The primary-key column on `related_entity`
#'       that `local_property` points to, or `NA` on the collection side.}
#'   }
#'   Entity types with no navigation properties are represented by a zero-row
#'   data frame.  Returns an empty list invisibly when the `$metadata` endpoint
#'   contains no `EntityType` elements.
#'
#' @seealso [awwid_join()] to join two tables using these relationships.
#' @export
#'
#' @examples
#' nav <- awwid_metadata_xml()
#'
#' # Inspect relationships for a specific entity
#' nav$WellReport
#'
#' # Find all entities that have at least one FK relationship
#' Filter(\(x) nrow(x) > 0 && any(!is.na(x$local_property)), nav)
awwid_metadata_xml <- function() {
  url <- paste0(
    "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata",
    "/$metadata"
  )

  resp <- httr2::request(url) |>
    httr2::req_headers(Accept = "application/xml") |>
    httr2::req_retry(
      max_tries = 15,
      is_transient = ~ httr2::resp_status(.x) %in% c(403, 429, 500, 502, 503),
      backoff = \(x) min(10 * 2^(x - 1), 120)
    ) |>
    httr2::req_perform()

  xml <- httr2::resp_body_string(resp) |> xml2::read_xml()

  # The EDM schema uses a default namespace; register it for XPath
  ns <- c(edm = "http://docs.oasis-open.org/odata/ns/edm")

  entity_types <- xml2::xml_find_all(xml, "//edm:EntityType", ns)

  if (length(entity_types) == 0L) {
    message("No EntityType elements found in the $metadata response.")
    return(invisible(list()))
  }

  result <- lapply(entity_types, function(et) {
    nav_nodes <- xml2::xml_find_all(et, "edm:NavigationProperty", ns)
    if (length(nav_nodes) == 0L) {
      return(data.frame(
        name                = character(),
        related_entity      = character(),
        local_property      = character(),
        referenced_property = character(),
        stringsAsFactors    = FALSE
      ))
    }

    # Strip the namespace-qualified prefix and any Collection(...) wrapper so
    # the entity name is readable (e.g. "Collection(GoA...Borehole)" -> "Borehole")
    types <- xml2::xml_attr(nav_nodes, "Type")
    related <- gsub("[()]", "", types)       # remove Collection( and trailing )
    related <- sub(".*\\.", "", related)     # keep only the final dotted segment

    # ReferentialConstraint holds the actual FK -> PK column mapping
    local_props <- vapply(nav_nodes, function(n) {
      rc <- xml2::xml_find_first(n, "edm:ReferentialConstraint", ns)
      if (inherits(rc, "xml_missing")) NA_character_
      else xml2::xml_attr(rc, "Property")
    }, character(1))

    ref_props <- vapply(nav_nodes, function(n) {
      rc <- xml2::xml_find_first(n, "edm:ReferentialConstraint", ns)
      if (inherits(rc, "xml_missing")) NA_character_
      else xml2::xml_attr(rc, "ReferencedProperty")
    }, character(1))

    data.frame(
      name                = xml2::xml_attr(nav_nodes, "Name"),
      related_entity      = related,
      local_property      = local_props,
      referenced_property = ref_props,
      stringsAsFactors    = FALSE
    )
  })

  entity_names <- xml2::xml_attr(entity_types, "Name")
  stats::setNames(result, entity_names)
}


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

  table_names <- metadata$value$name
  table_names <- table_names[!table_names %in% .awwid_excluded_tables]
  table_names
}
