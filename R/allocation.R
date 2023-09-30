#' Allocate wells
#'
#' @param lithologs a lithologs table derived from the `query_awwid_lithologs`
#'   function
#' @param screens a screens tabke derived from the `query_awwid_screens`
#'   function
#' @param model a `vetiver` trained machine learning model that returns a
#'   data.frame like object with the predicted response as a '.pred_class'
#'   column
#'
#' @return tibble of allocated screens
#' @export
allocate = function(lithologs, screens, model) {
  # prepare lithologs for ml prediction model
  lithologs = data.table::copy(lithologs)
  lithologs = na.omit(lithologs, cols = c("latitude", "longitude"))

  # model prediction
  predicted_intervals =
    predict(model, lithologs |> dplyr::group_by(.data$gicwellid)) |>
    data.table::as.data.table()

  # pick bedrock top and join with labelled lithologs
  labelled_intervals = cbind(predicted_intervals, lithologs)
  predicted_tops = pick_bedrock(labelled_intervals)
  predicted_logs = predicted_tops[labelled_intervals, on = "gicwellid"]

  # create a picks table that indicates whether a litholog was present
  predicted_picks = predicted_logs[, .SD[1], by = "gicwellid"]
  predicted_picks = predicted_picks[, .SD, .SDcols = c("gicwellid", "bedrock_dep")]
  predicted_picks[, "litholog_present" := TRUE]

  # join predicted picks with the screens
  screens_picked = screens |>
    units::drop_units() |>
    merge(predicted_picks, by = "gicwellid", all.x = TRUE)
  screens_picked[is.na(get("litholog_present")), "litholog_present" := FALSE]

  # extract missing values from interpolated surface
  board_ags = pins::board_url(c(dtb = "https://static.ags.aer.ca/files/document/DIG/DIG_2023_0014.zip"))
  dtb = board_ags |> pins::pin_download("dtb")
  unzip(dtb, exdir = tempdir())
  dtb = terra::rast(file.path(tempdir(), "DIG_2023_0014", "depth_to_bedrock_10tm.tif"))

  screens_picked_prj = screens_picked |>
    terra::vect(geom = c("longitude", "latitude"), crs = "epsg:4326") |>
    terra::project("epsg:3402")

  screens_picked_prj$dtb =
    terra::extract(dtb, screens_picked_prj, ID = FALSE)[[1]]

  screens_picked_df = screens_picked_prj |>
    as.data.frame() |>
    data.table::as.data.table()

  screens_picked_df[, "bedrock_dep_source" := data.table::fifelse(get("litholog_present") == TRUE, "nlp", "interpolation")]
  screens_picked_df[get("litholog_present") == FALSE, "bedrock_dep" := get("dtb")]

  # allocated each completion interval to surficial or bedrock
  screens_completions = data.table::copy(screens_picked_df)
  screens_completions[, "screen_thickness" := get("screendepthto") - get("screendepthfrom")]

  screens_completions[, "completed_top" := data.table::fifelse(
    get("screendepthfrom") < get("bedrock_dep") | is.na(get("bedrock_dep")),
    "surficial",
    "bedrock"
  )]
  screens_completions[, "completed_bot" := data.table::fifelse(
    get("screendepthto") <= get("bedrock_dep") | is.na(get("bedrock_dep")),
    "surficial",
    "bedrock"
  )]

  screens_completions[, "surficial_thickness" := data.table::fifelse(
    get("bedrock_dep") - get("screendepthfrom") > 0,
    get("bedrock_dep") - get("screendepthfrom"), 0
  )]

  screens_completions[, "bedrock_thickness" := data.table::fifelse(
    get("screendepthto") - get("bedrock_dep") > 0,
    get("screendepthto") - get("bedrock_dep"), 0
  )]

  screens_completions[, c("screen_thickness", "surficial_thickness", "bedrock_thickness") := NULL]
  screens_completions = screens[, .SD, .SDcols = c("gicwellid", "latitude", "longitude")][
    screens_completions, on = "gicwellid"]

  return(screens_completions)
}
