add_projected_coords <- function(obj) {
  obj <- obj |>
    terra::vect(geom = c("longitude", "latitude"), crs = "epsg:4326", keepgeom = TRUE) |>
    terra::project("epsg:3402")

  obj$x <- terra::crds(obj)[, 1]
  obj$y <- terra::crds(obj)[, 2]
  obj <- obj |>
    as.data.frame() |>
    dplyr::as_tibble()
  return(obj)
}

#' Snap bedrock picks to the log intervals and assign a unit column to either
#' 'Bedrock' or 'Surficial'
#'
#' Bedrock top picks, often made in 'Viewlog' are not aligned precisely with the
#' litholog intervals; they often float slightly above or below the interval
#' boundaries. This function finds the closest interval boundary (top or bottom)
#' depth to the pick and moves the pick to align precisely with the boundary.
#'
#' @param lithologs a data.frame/tibble of litholog intervals with 'int_top_dep'
#'   representing the interval top depths, and 'bedrock_dep' defining the picked
#'   bedrock depth in the log.
#'
#' @return a tibble with the modified bedrock depths
#' @export
lithologs_snap <- function(lithologs) {
  snapped_bedrock <- lithologs |>
    dplyr::group_by(.data$gicwellid) |>
    dplyr::mutate(diff = abs(.data$int_top_dep - .data$bedrock_dep)) |>
    dplyr::filter(diff == min(.data$diff)) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::mutate(bedrock_dep_snapped = dplyr::if_else(
      abs(.data$int_top_dep - .data$bedrock_dep) <
        abs(.data$int_bot_dep - .data$bedrock_dep),
      .data$int_top_dep,
      .data$int_bot_dep
    )) |>
    dplyr::select("gicwellid", "bedrock_dep_snapped")

  lithologs <- lithologs |>
    dplyr::left_join(snapped_bedrock) |>
    dplyr::mutate(
      bedrock_dep = as.numeric(.data$bedrock_dep),
      bedrock_dep_snapped = as.numeric(.data$bedrock_dep_snapped),
      bedrock_dep = dplyr::if_else(
        !is.na(.data$bedrock_dep_snapped),
        .data$bedrock_dep_snapped,
        .data$bedrock_dep
      )
    ) |>
    dplyr::select(-"bedrock_dep_snapped")

  return(lithologs)
}

#' Assign litholog intervals to 'Bedrock' or 'Surficial' units
#'
#' @param lithologs tibble of litholog data with 'int_top_dep' and 'bedrock_dep'
#'   columns
#'
#' @return tibble with a new 'unit' column containing a factor with the levels =
#'   c("Bedrock", "Surficial")
#' @export
lithologs_assign <- function(lithologs) {
  lithologs |>
    dplyr::mutate(
      unit = dplyr::if_else(
        .data$int_top_dep >= .data$bedrock_dep,
        "Bedrock",
        "Surficial"
      ),
      unit = factor(.data$unit, levels = c("Bedrock", "Surficial"))
    )
}

#' Join the bedrock picks with the lithologs
#'
#' The ‘gicwellid’ field in the lithologs will represent ‘LOC_NAME_ALT2’ for
#' logs coming from ABGEOL, and will represent the GIC_WELL_ID for those coming
#' from AWWID, and also contains UWIs and other local names etc. This was
#' because original GFM pick compilation had a single gicwellid column that used
#' LOC_NAME_ALT2 as the identifier for picks made from ABGEOL, so it is the only
#' column that can be used to relate the picks back to the logs.
#'
#' Unfortunately, this identifier is not entirely unique and there are some
#' duplicated ids.
#'
#' This function joins the picks with the lithologs based on 'gicwellid', but also
#' removes joined locations where the pick coordinates are not within 500 m of
#' the litholog coordinates. This should not remove more than 3000 picks out of
#' ~ 130,000 otherwise an error is raised.
#'
#' @param lithologs a tibble of compiled lithologs
#' @param picks a tibble of compiled picks
#'
#' @return the joined data
#' @export
lithologs_join <- function(lithologs, picks) {
  # convert coordinates
  lithologs <- add_projected_coords(lithologs)
  picks <- add_projected_coords(picks)

  # join logs with picks
  picks_df <- picks |>
    dplyr::select(c("gicwellid", "x", "y", "bedrock_dep")) |>
    dplyr::rename(picks_x = "x", picks_y = "y")

  lithologs_labelled <- dplyr::left_join(
    lithologs,
    picks_df,
    by = dplyr::join_by("gicwellid")
  )

  # set joined columns to NA if the coordinates do not match within tolerance
  lithologs_labelled <- lithologs_labelled |>
    dplyr::mutate(
      dist = sqrt((.data$x - .data$picks_x)^2 + (.data$y - .data$picks_y)^2),
      bedrock_dep = dplyr::if_else(
        .data$dist < 1000,
        .data$bedrock_dep,
        NA_real_
      )
    ) |>
    dplyr::select(-c("picks_x", "picks_y", "x", "y"))

  return(lithologs_labelled)
}

#' Function to determine the depth to bedrock based on litholog intervals that
#' are labelled as either 'Bedrock' or 'Surficial'
#'
#' This function finds the first occurrence of 'Bedrock' in the log and takes
#' this at the bedrock top, even if other intervals labelled as 'Surficial'
#' occur beneath first bedrock interval. As such, this represents a minimum
#' estimate for depth to bedrock and can be affected by glaciotectonic rafts
#' etc.
#'
#' @param lithologs tibble of litholog data containing the column '.pred_class'
#'   which as two factor levels, 'Bedrock' and 'Surficial'.
#' @param response character, name of column with the labels (either "Bedrock"
#'   or "Surficial")
#'
#' @return tibble containing the bedrock depths per well, with 'gicwellid' and
#'   '.bedrock_dep' columns.
#' @export
pick_bedrock_first <- function(lithologs, response = ".pred_class") {
  first_bedrock <- lithologs |>
    dplyr::group_by(.data$gicwellid) |>
    dplyr::filter(
      !!rlang::sym(response) == "Bedrock",
      !duplicated(!!rlang::sym(response))
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(.bedrock_dep = "int_top_dep") |>
    dplyr::select("gicwellid", ".bedrock_dep")

  return(first_bedrock)
}

#' Function to determine the depth to bedrock based on litholog intervals that
#' are labelled as either 'Bedrock' or 'Surficial'
#'
#' This function finds the first occurrence of 'Bedrock' that is below any other
#' intervals labelled as 'Surficial' in each log. As such, this represents a
#' maximum estimate of bedrock depth; it avoids the problems with glaciotectonic
#' rafts of bedrock, but can potentially overestimate bedrock depth if some
#' intervals are misclassified as surficial with the bedrock strata.
#'
#' @param lithologs tibble of litholog data containing the column '.pred_class'
#'   which as two factor levels, 'Bedrock' and 'Surficial'.
#' @param response character, name of the response variable column, default
#' is '.pred_class'
#'
#' @return tibble containing the bedrock depths per well, with 'gicwellid' and
#'   '.bedrock_dep' columns.
#' @export
pick_bedrock_last <- function(lithologs, response = ".pred_class") {
  # get the maximum depth of any units that are classified as surficial
  max_surf <- lithologs |>
    dplyr::group_by(.data$gicwellid) |>
    dplyr::filter(!!rlang::sym(response) == "surficial") |>
    dplyr::summarise(minv = max(.data$int_top_dep))

  # take the top of the next interval beneath any surficial as the bedrock top
  y_pred <- lithologs |>
    dplyr::left_join(max_surf) |>
    dplyr::group_by(.data$gicwellid) |>
    dplyr::filter(
      .data$int_top_dep > .data$minv | is.na(.data$minv),
      !!rlang::sym(response) == "Bedrock"
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename(.bedrock_dep = "int_top_dep") |>
    dplyr::select("gicwellid", ".bedrock_dep")

  return(y_pred)
}

#' Create a new bedrock top pick dataset from a litholog table that includes
#' a 'bedrock_dep' column
#'
#' @param predictions tibble
#'
#' @return a tibble with the well identifier, coordinates, ground elevation,
#'   the 'pick_date', and the 'ags_source' column assigned to 'NLP autopicked'
#'   and 'source_type'
#' @export
picks_create_table <- function(predictions) {
  # determine the bedrock depths
  depths <- pick_bedrock_last(predictions)
  predictions <- dplyr::inner_join(predictions, depths)
  predictions <- predictions |>
    dplyr::rename(bedrock_dep = ".bedrock_dep")

  # create a pick table
  new_picks <- predictions |>
    dplyr::filter(!is.na(.data$bedrock_dep)) |>
    dplyr::select("gicwellid", "longitude", "latitude", "gr_elev", "bedrock_dep") |>
    dplyr::mutate(
      data_type = "Borehole",
      ags_source = "NLP autopicked",
      pick_date = Sys.Date(),
      source_type = "Water well",
      stratigraphy = "Bedrock top",
      data_source = "Water well",
      bedrock_elev = .data$gr_elev - .data$bedrock_dep
    ) |>
    dplyr::distinct(.data$gicwellid, .keep_all = TRUE)

  return(new_picks)
}
