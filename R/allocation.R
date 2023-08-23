#' Allocate wells
#'
#' @param lithologs a lithologs table derived from the `query_awwid_lithologs`
#'   function
#' @param screens a screens tabke derived from the `query_awwid_screens`
#'   function
#' @param model a `vetiver` trained machine learning model
#'
#' @return tibble of allocated screens
#' @export
allocate <- function(lithologs, screens, model) {
  # prepare lithologs for ml prediction model
  lithologs <- lithologs |>
    tidyr::drop_na(c("latitude", "longitude")) |>
    dplyr::group_by(.data$gicwellid)

  # model prediction
  predicted_intervals <- predict(model, lithologs)
  predicted_intervals <- dplyr::bind_cols(predicted_intervals, lithologs)

  predicted_tops <-
    bedrocktopo::pick_bedrock_last(predicted_intervals) |>
    dplyr::rename(bedrock_dep = ".bedrock_dep")

  predicted_logs <- dplyr::left_join(predicted_intervals, predicted_tops,
                                     by = dplyr::join_by("gicwellid"))

  predicted_picks <- predicted_logs |>
    dplyr::group_by(.data$gicwellid) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select("gicwellid", "bedrock_dep") |>
    dplyr::mutate(litholog_present = TRUE)

  # join predicted picks with the screens
  screens_picked <- screens |>
    units::drop_units() |>
    dplyr::left_join(predicted_picks, by = "gicwellid") |>
    dplyr::mutate(litholog_present = tidyr::replace_na(.data$litholog_present, FALSE))

  # extract missing values from interpolated surface
  endp <- AzureStor::blob_endpoint(
    endpoint = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
    key = Sys.getenv("AZURE_KEY")
  )
  bl <- AzureStor::blob_container(endp, "pins")
  board <- pins::board_azure(bl, "near-surface")

  dtb <- board |>
    pins::pin_download("dtb-prov") |>
    terra::rast()

  screens_picked_prj <- screens_picked |>
    terra::vect(geom = c("longitude", "latitude"), crs = "epsg:4326") |>
    terra::project("epsg:3402")

  screens_picked_prj$dtb <-
    terra::extract(dtb, screens_picked_prj, ID = FALSE)[[1]]

  screens_picked_df <- screens_picked_prj |>
    as.data.frame() |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      bedrock_dep_source = dplyr::if_else(.data$litholog_present == TRUE, "nlp", "interpolation"),
      bedrock_dep = dplyr::if_else(.data$litholog_present == TRUE, .data$bedrock_dep, .data$dtb)
    )

  # allocated each completion interval to surficial or bedrock
  screens_completions <- screens_picked_df |>
    dplyr::mutate(
      screen_thickness = .data$screendepthto - .data$screendepthfrom,
      completed_top = dplyr::if_else(
        .data$screendepthfrom < .data$bedrock_dep | is.na(.data$bedrock_dep),
        "surficial",
        "bedrock"
      ),
      completed_bot = dplyr::if_else(.data$screendepthto <= .data$bedrock_dep | is.na(.data$bedrock_dep), "surficial", "bedrock"),
      surficial_thickness = dplyr::if_else(
        .data$bedrock_dep - .data$screendepthfrom > 0,
        .data$bedrock_dep - .data$screendepthfrom, 0
      ),
      bedrock_thickness = dplyr::if_else(
        .data$screendepthto - .data$bedrock_dep > 0,
        .data$screendepthto - .data$bedrock_dep,
        0
      )
      # prop_surficial = .data$surficial_thickness / (.data$surficial_thickness + .data$bedrock_thickness)
    ) |>
    dplyr::select(-c(
      "screen_thickness",
      "surficial_thickness",
      "bedrock_thickness"
    ))

  return(screens_completions)
}
