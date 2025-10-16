#' Predefined query to extract lithologs from the AWWID 'wells' and
#' 'lithologies' tables
#'
#' @param wells tibble of the 'wells' table from AWWID. The 'gicwellid',
#'   'wellid', 'longitude', and 'latitude' columns have to be present.
#' @param well_reports tibble of the 'wellreports' table from AWWID. Only the
#'   columns 'wellreportid', 'wellid', and 'totaldepthdrilled' are required.
#' @param lithologies tibble of the 'lithologies' table from AWWID. The
#'   'wellreportid', 'material', 'description', 'lithdepthfrom', 'lithdepthto',
#'   'colour', 'waterbearing' columns are required.
#'
#' @return tibble of processed AWWID litholog data
#' @export
#' @importFrom rlang abort
#' @importFrom glue glue
#' @importFrom dplyr rename rename_with select left_join join_by na_if as_tibble
#'   any_of all_of contains
#' @importFrom tidyr drop_na
#' @importFrom units drop_units
query_lithologs <- function(wells, well_reports, lithologies) {
  # Check the required columns are present in "wells"
  required_well_cols <- c("gicwellid", "wellid", "longitude", "latitude")
  check_wells <- required_well_cols %in% names(wells)

  if (!all(check_wells)) {
    missing <- required_well_cols[!check_wells]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `wells` table is missing the column(s): {missing}"))
  }

  # Check the required columns are present in "well_reports"
  required_report_cols <- c("wellid", "wellreportid", "totaldepthdrilled")
  check_reports <- required_report_cols %in% names(well_reports)

  if (!all(check_reports)) {
    missing <- required_report_cols[!check_reports]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `well_reports` table is missing the column(s): {missing}"))
  }

  # Check that "well_reports" is metricated
  if (!inherits(well_reports$totaldepthdrilled, "units")) {
    abort("The `well_reports` tibble must be metricated. Use the `metricate()` function.")
  }

  # Check the required columns are present in "lithologies"
  required_lith_cols <- c(
    "wellreportid",
    "material",
    "description",
    "lithdepthfrom",
    "lithdepthto",
    "colour",
    "waterbearing"
  )

  check_lithologies <- required_lith_cols %in% names(lithologies)

  if (!all(check_lithologies)) {
    missing <- required_lith_cols[!check_lithologies]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `lithologies` tibble is missing the column(s): {missing}"))
  }

  # Check that "lithologies" is metricated
  if (!inherits(lithologies$lithdepthfrom, "units") ||
      !inherits(lithologies$lithdepthto, "units")) {
    abort("The `lithologies` tibble must be metricated. Use the `metricate()` function.")
  }

  # Prepare AWWID tables
  awwid_wells <- wells |>
    rename_with(tolower) |>
    select(c("gicwellid", "wellid", "longitude", "latitude")) |>
    drop_na(c("longitude", "latitude"))

  well_reports_index <- well_reports |>
    select(all_of(c("wellid", "wellreportid", "totaldepthdrilled")))

  awwid_lithologies <- left_join(
    lithologies,
    well_reports_index,
    by = join_by("wellreportid")
  )

  awwid <-
    left_join(awwid_lithologies, awwid_wells, by = "wellid") |>
    select(-any_of(c("wellreportid", "lithologyid", contains("time"), "wellid")))

  # standardize columns
  awwid <- awwid |>
    mutate(
      material = na_if(.data$material, ""),
      description = na_if(.data$description, "")
    ) |>
    rename(
      int_top_dep = "lithdepthfrom",
      int_bot_dep = "lithdepthto",
      bh_depth = "totaldepthdrilled",
      material_desc = "description"
    ) |>
    drop_units()

  # add ground elevation
  awwid <- add_ground_elevation(awwid)

  # reorder columns
  col_order <- c(
    "gicwellid",
    "longitude",
    "latitude",
    "gr_elev",
    "bh_depth",
    "int_top_dep",
    "int_bot_dep",
    "material",
    "material_desc",
    "colour",
    "waterbearing"
  )

  awwid <- awwid |>
    select(!!!col_order) |>
    as_tibble()

  # Join with materials
  awwid <- left_join(
    awwid,
    materials,
    by = join_by("material")
  )

  return(awwid)
}


#' Predefined query to extract the screen and perforation inteval ranges frp,
#' the AWWID 'wells', 'wellreports', 'lithologies', 'screens' and 'perforations'
#' tables
#'
#' @param wells tibble of the 'wells' table from AWWID
#' @param wells_reports tibble of the 'wellreports' table from AWWID
#' @param screens tibble of the 'screens' table from AWWID
#' @param perforations tibble of the 'perforations' table from AWWID
#' @param .aggregate logical. If TRUE (default) then the full depth range of
#'  screens/perforations for each well is returned. If FALSE, then all the
#'  individual screen/perforation intervals are returned.
#' @param .assumed_top numeric. If a well has no screens or perforations,
#'   then a 5 m screen interval is created at the total depth drilled. This
#'   parameter sets the length of that interval in metres.
#'
#' @return tibble of processed AWWID litholog data
#' @export
#' @importFrom dplyr rename select left_join join_by bind_rows group_by summarize
#'  first ungroup mutate as_tibble filter distinct
#' @importFrom tidyr drop_na
#' @importFrom units as_units
query_screens <- function(wells, wells_reports, screens, perforations, .aggregate = TRUE,
                          .assumed_top = 5) {
  # Check that required "wells" columns are present
  required_well_cols <- c("gicwellid", "wellid", "longitude", "latitude")
  check_wells <- required_well_cols %in% names(wells)
  if (!all(check_wells)) {
    missing <- required_well_cols[!check_wells]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `wells` table is missing the column(s): {missing}"))
  }

  # Check that required "wells_reports" columns are present
  required_report_cols <- c("wellid", "wellreportid", "totaldepthdrilled")
  check_reports <- required_report_cols %in% names(wells_reports)
  if (!all(check_reports)) {
    missing <- required_report_cols[!check_reports]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `wells_reports` table is missing the column(s): {missing}"))
  }

  # Check that "wells_reports" is metricated
  if (!inherits(wells_reports$totaldepthdrilled, "units")) {
    abort("The `wells_reports` tibble must be metricated. Use the `metricate()` function.")
  }

  # Check that required "screens" columns are present
  required_screens_cols <- c("wellreportid", "screendepthfrom", "screendepthto")
  check_screens <- required_screens_cols %in% names(screens)
  if (!all(check_screens)) {
    missing <- required_screens_cols[!check_screens]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `screens` table is missing the column(s): {missing}"))
  }

  # Check that "screens" is metricated
  if (!inherits(screens$screendepthfrom, "units") ||
      !inherits(screens$screendepthto, "units")) {
    abort("The `screens` tibble must be metricated. Use the `metricate()` function.")
  }

  # Check that required "perforations" columns are present
  required_perfs_cols <- c("wellreportid", "perfdepthfrom", "perfdepthto")
  check_perfs <- required_perfs_cols %in% names(perforations)
  if (!all(check_perfs)) {
    missing <- required_perfs_cols[!check_perfs]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `perforations` table is missing the column(s): {missing}"))
  }

  # Check that "perforations" is metricated
  if (!inherits(perforations$perfdepthfrom, "units") ||
      !inherits(perforations$perfdepthto, "units")) {
    abort("The `perforations` tibble must be metricated. Use the `metricate()` function.")
  }

  # Rename perforations columns to match the names of the screens
  perfs <- perforations |>
    rename(screendepthfrom = "perfdepthfrom", screendepthto = "perfdepthto") |>
    select("wellreportid":"screendepthto")

  # create a lookup table to relate gic_well_id to the well_report_id
  wells_index <- wells |>
    select(c("wellid", "gicwellid", "longitude", "latitude"))

  wells_reports_index <- wells_reports |>
    select(c("wellid", "wellreportid"))

  linking <- left_join(
    wells_reports_index,
    wells_index,
    by = join_by("wellid")
    ) |>
    select(-"wellid")

  # combine the perforations and screens
  perfs <- left_join(perfs, linking, by = join_by("wellreportid"))
  screens <- left_join(screens, linking, by = join_by("wellreportid"))
  screens_perfs <- bind_rows(screens, perfs)

  # aggregate the full depth range of screens/perfs for each well
  if (.aggregate) {
    screens_perfs <- screens_perfs |>
      group_by(.data$gicwellid) |>
      summarize(
        wellreportid = first(.data$wellreportid, na_rm = TRUE),
        longitude = first(.data$longitude, na_rm = TRUE),
        latitude = first(.data$latitude, na_rm = TRUE),
        screendepthfrom = min(.data$screendepthfrom, na.rm = TRUE),
        screendepthto = max(.data$screendepthto, na.rm = TRUE)
      ) |>
      ungroup() |>
      mutate(
        screendepthmid = .data$screendepthfrom + ((.data$screendepthto - .data$screendepthfrom) / 2)
      ) |>
      as_tibble()
  } else {
    screens_perfs <- screens_perfs |>
      select(all_of(c("wellreportid", "longitude", "latitude", "screendepthfrom", "screendepthto"))) |>
      mutate(
        screendepthmid = .data$screendepthfrom + ((.data$screendepthto - .data$screendepthfrom) / 2)
      ) |>
      as_tibble()
  }

  # for wells with no screens/perfs, use the total depth drilled instead
  missing_screens <- wells_reports |>
    filter(!.data$wellreportid %in% screens_perfs$wellreportid) |>
    select("wellreportid", "totaldepthdrilled", "wellid") |>
    rename(screendepthto = "totaldepthdrilled") |>
    mutate(
      screendepthfrom = .data$screendepthto - as_units(.assumed_top, "m"),
      screendepthmid = .data$screendepthto - as_units(.assumed_top / 2, "m")
    ) |>
    drop_na("screendepthto") |>
    distinct(.data$wellid, .keep_all = TRUE)

  missing_screens <-
    left_join(
      missing_screens,
      select(wells, c("wellid", "gicwellid", "latitude", "longitude")),
      by = "wellid"
    ) |>
    select(-"wellid") |>
    filter(.data$screendepthto > as_units(0, "m"))

  screens_merged <-
    bind_rows(screens_perfs, missing_screens) |>
    distinct(.data$gicwellid, .keep_all = TRUE) |>
    drop_na(c("gicwellid", "latitude", "longitude"))

  col_order <- c(
    "gicwellid",
    "wellreportid",
    "longitude",
    "latitude",
    "screendepthfrom",
    "screendepthto",
    "screendepthmid"
  )
  screens_merged <- screens_merged |>
    select(!!!col_order) |>
    as_tibble()

  return(screens_merged)
}


#' Predefined query to extract a table of static water levels
#'
#' @param wells tibble of 'wells' data that has been metricated. The only
#'   columns that are required are c('wellid', 'gicwellid') but usually you
#'   would also want 'latitude' and 'longitude'.
#' @param well_reports tibble of 'wellreports' data that has been metricated.
#'   The required columns are c("wellid", "wellreportid").
#' @param pump_tests tibble of 'pumptests' data that has been metricated.
#'   Columns that are required in the pump tests download are c("wellreportid",
#'   "staticwaterlevel", "testdate")
#'
#' @return tibble
#' @export
#' @examples
#' wells <-
#'   request_awwid("wells", select = "wellid,gicwellid,longitude,latitude") |>
#'   metricate()
#'
#' well_reports <-
#'   request_awwid("wellreports", select = "wellid,wellreportid") |>
#'   metricate()
#'
#' pumptests <-
#'   request_awwid(
#'     "pumptests",
#'     select = "wellreportid,staticwaterlevel,testdate"
#' ) |>
#'   metricate()
#'
#' query_staticwater(wells, well_reports, pumptests) |>
#'   tidyr::drop_na(staticwaterlevel)
#' @importFrom rlang abort
#' @importFrom glue glue
query_staticwater <- function(wells, well_reports, pump_tests) {
  # check required columns
  pumptest_cols <- c("wellreportid", "staticwaterlevel", "testdate")
  check_cols <- pumptest_cols %in% names(pump_tests)

  if (!all(check_cols)) {
    missing <- pumptest_cols[!check_cols]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `pump_tests` tibble is missing the column(s): {missing}"))
  }

  reports_cols <- c("wellid", "wellreportid")
  check_cols <- reports_cols %in% names(well_reports)

  if (!all(check_cols)) {
    missing <- reports_cols[!check_cols]
    missing <- paste(missing, collapse = ", ")
    abort(glue("The `well_reports` tibble is missing the column(s): {missing}"))
  }

  pumptests <-
    dplyr::left_join(wells, well_reports) |>
    dplyr::left_join(pumptests)

  return(pumptests)
}


add_ground_elevation <- function(logs) {
  fp <- system.file("extdata/dem.tif", package = "ABwaterwells")
  dem <- terra::rast(fp)

  log_crds <- logs |>
    as.data.frame() |>
    terra::vect(geom = c("longitude", "latitude"), crs = "epsg:4326") |>
    terra::project("epsg:3402")

  logs$gr_elev <- terra::extract(dem, log_crds, ID = FALSE)[[1]]

  return(logs)
}
