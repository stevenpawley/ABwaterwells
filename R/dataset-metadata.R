#' AWWID table schemas
#'
#' A hand-curated list of schemas for the 25 tables available on the AWWID
#' OData service. Used internally by [awwid_tbl()] to validate table and column
#' names before making requests to the server.
#'
#' @format ## `metadata`
#' A named list with 25 elements, one per AWWID table (names are in
#' `PascalCase`, e.g. `"Wells"`, `"WellReports"`). Each element contains:
#' \describe{
#'   \item{title}{Human-readable table name.}
#'   \item{description}{Short description of the table's contents.}
#'   \item{columns}{A named list of column definitions. Each column is itself a
#'     list with two fields: `type` (R class of the column, e.g. `"integer"`,
#'     `"character"`, `"POSIXct"`) and `description` (plain-text description of
#'     the column).}
#'   \item{relations}{A list with two fields: `pk` (name of the primary key
#'     column, or a composite key joined by `"_"`) and optionally `fk` (a
#'     character vector of foreign key column names).}
#' }
#' @source Hand-curated from the AWWID OData service metadata endpoint
#'   `https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata`.
#'   See `data-raw/metadata.R` for the generation script.
"metadata"
