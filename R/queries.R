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
query_lithologs <- function(wells, well_reports, lithologies) {
  # check the required columns are present
  check_wells <-
    c("gicwellid", "wellid", "longitude", "latitude") %in% names(wells)

  if (!all(check_wells)) {
    missing <- c("gicwellid", "wellid", "longitude", "latitude")[!check_wells]
    missing <- paste(missing, collapse = ", ")
    rlang::abort(glue::glue(
      "The `wells` tibble is missing the required column(s): {missing}"
    ))
  }

  check_reports <-
    c("wellid", "wellreportid", "totaldepthdrilled") %in% names(well_reports)

  if (!all(check_reports)) {
    missing <- c("wellid", "wellreportid", "totaldepthdrilled")[!check_reports]
    missing <- paste(missing, collapse = ", ")
    rlang::abort(glue::glue(
      "The `well_reports` tibble is missing the required column(s): {missing}"
    ))
  }

  req_lith_cols <- c(
    "wellreportid",
    "material",
    "description",
    "lithdepthfrom",
    "lithdepthto",
    "colour",
    "waterbearing"
  )

  check_lithologies <- req_lith_cols %in% names(lithologies)

  if (!all(check_lithologies)) {
    missing <- req_lith_cols[!check_lithologies]
    missing <- paste(missing, collapse = ", ")
    rlang::abort(glue::glue(
      "The `lithologies` tibble is missing the required column(s): {missing}"
    ))
  }

  # prepare AWWID tables
  awwid_wells <- wells |>
    dplyr::rename_with(tolower) |>
    dplyr::select(c("gicwellid", "wellid", "longitude", "latitude")) |>
    tidyr::drop_na(c("longitude", "latitude"))

  awwid_lithologies <- dplyr::left_join(
    lithologies,
    dplyr::select(
      well_reports,
      c("wellid", "wellreportid", "totaldepthdrilled")
    ),
    by = dplyr::join_by("wellreportid")
  )

  # join well table with lithologies
  awwid <-
    dplyr::left_join(awwid_lithologies, awwid_wells, by = "wellid") |>
    dplyr::select(-dplyr::any_of(
      c("wellreportid",
        "lithologyid",
        dplyr::contains("time"),
        "wellid"
      )
    ))

  # standardize columns
  awwid <- awwid |>
    dplyr::mutate(
      material = dplyr::na_if(.data$material, ""),
      description = dplyr::na_if(.data$description, "")
    ) |>
    dplyr::rename(
      int_top_dep = "lithdepthfrom",
      int_bot_dep = "lithdepthto",
      bh_depth = "totaldepthdrilled",
      material_desc = "description"
    ) |>
    units::drop_units()

  # remove logs containing intervals of zero thickness
  invalid_ids <- awwid |>
    dplyr::filter(
      .data$int_top_dep == .data$int_bot_dep | .data$int_top_dep < 0
    ) |>
    dplyr::pull("gicwellid") |>
    unique()

  awwid <- awwid |>
    dplyr::filter(!.data$gicwellid %in% invalid_ids)

  # check for distinct depths
  awwid <- awwid |>
    dplyr::distinct(.data$gicwellid, .data$int_top_dep, .keep_all = TRUE)

  # add ground elevation
  awwid <- add_ground_elevation(awwid)

  # add other standard columns
  awwid <- awwid |>
    dplyr::mutate(
      well_type = as.factor("Vertical"),
      location_type = as.factor("Well"),
      location_source = as.factor("Aenv database")
    )

  # reorder columns
  col_order <- c(
    "gicwellid",
    "longitude",
    "latitude",
    "gr_elev",
    "bh_depth",
    "well_type",
    "location_type",
    "location_source",
    "int_top_dep",
    "int_bot_dep",
    "material",
    "material_desc",
    "colour",
    "waterbearing"
  )
  awwid <- awwid |>
    dplyr::select(!!!col_order) |>
    dplyr::as_tibble()

  return(awwid)
}

#' Predefined query to extract the screen and perforation inteval ranges frp,
#' the AWWID 'wells', 'wellreports', 'lithologies', 'screens' and 'perforations'
#' tables
#'
#' @param wells dataframe of the 'wells' table from AWWID
#' @param wells_reports dataframe of the 'wellreports' table from AWWID
#' @param screens dataframe of the 'screens' table from AWWID
#' @param perforations dataframe of the 'perforations' table from AWWID
#'
#' @return tibble of processed AWWID litholog data
#' @export
query_screens <-
  function(
    wells,
    wells_reports,
    screens,
    perforations) {

    # rename perforations columns to match the names of the screens
    # because we are going to combine these two tables
    perfs <- perforations |>
      dplyr::rename(
        screendepthfrom = "perfdepthfrom",
        screendepthto = "perfdepthto"
      ) |>
      dplyr::select("wellreportid":"screendepthto")

    # create a lookup table to relate gic_well_id to the well_report_id
    linking <- wells_reports |>
      dplyr::select(c("wellid", "wellreportid")) |>
      dplyr::left_join(
        wells |>
          dplyr::select(c("wellid", "gicwellid", "longitude", "latitude")),
        by = dplyr::join_by("wellid")
      ) |>
      dplyr::select(-"wellid")

    # combine the perforations and screens
    perfs <- dplyr::left_join(
      perfs,
      linking,
      by = dplyr::join_by("wellreportid")
    )

    screens <- dplyr::left_join(
      screens,
      linking,
      by = dplyr::join_by("wellreportid")
    )

    screens_perfs <- dplyr::bind_rows(screens, perfs)

    # aggregate the full depth range of screens/perfs for each well
    screens_avg <- screens_perfs |>
      dtplyr::lazy_dt() |>
      dplyr::group_by(.data$gicwellid) |>
      dplyr::summarize(
        wellreportid = dplyr::first(.data$wellreportid, na_rm = TRUE),
        longitude = dplyr::first(.data$longitude, na_rm = TRUE),
        latitude = dplyr::first(.data$latitude, na_rm = TRUE),
        screendepthfrom = min(.data$screendepthfrom, na.rm = TRUE),
        screendepthto = max(.data$screendepthto, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        screendepthmid = .data$screendepthfrom +
          ((.data$screendepthto - .data$screendepthfrom) / 2)) |>
      dplyr::as_tibble()

    # for wells with no screens/perfs, use the total depth drilled instead
    missing_screens <- wells_reports |>
      dplyr::filter(!.data$wellreportid %in% screens_avg$wellreportid) |>
      dplyr::select("wellreportid", "totaldepthdrilled", "wellid") |>
      dplyr::rename(screendepthto = "totaldepthdrilled") |>
      tidyr::drop_na("screendepthto") |>
      dplyr::distinct(.data$wellid, .keep_all = TRUE)

    missing_screens <-
      dplyr::left_join(
        missing_screens,
        dplyr::select(wells, c("wellid", "gicwellid", "latitude", "longitude")),
        by = "wellid"
      ) |>
      dplyr::select(-"wellid") |>
      dplyr::filter(.data$screendepthto > units::as_units(0, "m"))

    screens_merged <-
      dplyr::bind_rows(screens_avg, missing_screens) |>
      dplyr::distinct(.data$gicwellid, .keep_all = TRUE) |>
      tidyr::drop_na(c("gicwellid", "latitude", "longitude"))

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
query_staticwater <- function(wells, well_reports, pump_tests) {
  # check required columns
  pumptest_cols <- c(
    "wellreportid",
    "staticwaterlevel",
    "testdate"
  )
  check_cols <- pumptest_cols %in% names(pump_tests)

  if (!all(check_cols)) {
    missing <- pumptest_cols[!check_cols]
    missing <- paste(missing, collapse = ", ")
    rlang::abort(glue::glue(
      "The `pump_tests` tibble is missing the required column(s): {missing}"
    ))
  }

  reports_cols <- c("wellid", "wellreportid")
  check_cols <- reports_cols %in% names(well_reports)

  if (!all(check_cols)) {
    missing <- reports_cols[!check_cols]
    missing <- paste(missing, collapse = ", ")
    rlang::abort(glue::glue(
      "The `well_reports` tibble is missing the required column(s): {missing}"
    ))
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
