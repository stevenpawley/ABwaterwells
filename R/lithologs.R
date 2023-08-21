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
