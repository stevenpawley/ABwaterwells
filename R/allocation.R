allocate <- function(lithologs, screens, model) {
  # model prediction method
  lithologs <- lithologs
  dplyr::group_by(.data$id)

  predicted_intervals <- predict(model, lithologs)
  predicted_intervals <- dplyr::bind_cols(predicted_intervals, lithologs)
  predicted_tops <- pick_bedrock_last(predicted_intervals) |>
    dplyr::rename(bedrock_dep = ".bedrock_dep")
  predicted_logs <- dplyr::left_join(predicted_intervals, predicted_tops)

  predicted_picks <- predicted_logs |>
    dplyr::group_by(.data$gic_well_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select("gic_well_id", "bedrock_dep") |>
    dplyr::mutate(litholog_present = !is.na(.data$bedrock_dep))

  screens_picked <- dplyr::left_join(screens, predicted_picks, by = "gic_well_id")

  screens_picked <- dplyr::left_join(
    screens_picked,
    wells |> dplyr::select("gic_well_id", "latitude", "longitude"),
    by = "gic_well_id"
  )

  dtb <- get_container("pins") |>
    pins::board_azure("near-surface") |>
    pins::pin_download("dtb-prov") |>
    terra::rast()

  screens_picked <- screens_picked |>
    terra::vect(geom = c("longitude", "latitude"), crs = "epsg:4326") |>
    terra::project("epsg:3402")

  screens_picked$dtb <-
    terra::extract(dtb, screens_picked, ID = FALSE)[[1]]
  screens_picked <- screens_picked |>
    dplyr::mutate(
      bedrock_dep_source = dplyr::if_else(is.na(bedrock_dep), "interpolation", "nlp"),
      bedrock_dep = dplyr::if_else(is.na(bedrock_dep), dtb, bedrock_dep)
    )

  screens_picked <-
    as.data.frame(screens_picked) |>
    dplyr::select(-dtb) |>
    dplyr::as_tibble()

  screens_completions <-
    screens_picked |>
    dplyr::mutate(
      completed_top = dplyr::if_else(screen_top_dep < bedrock_dep, "surficial", "bedrock"),
      completed_bot = dplyr::if_else(screen_bot_dep <= bedrock_dep, "surficial", "bedrock"),
      screen_thickness = screen_bot_dep - screen_top_dep,
      surficial_thickness = dplyr::if_else(
        bedrock_dep - screen_top_dep > 0,
        bedrock_dep - screen_top_dep,
        0
      ),
      bedrock_thickness = dplyr::if_else(
        screen_bot_dep - bedrock_dep > 0,
        screen_bot_dep - bedrock_dep,
        0
      ),
      prop_surficial = surficial_thickness / (surficial_thickness + bedrock_thickness)
    ) |>
    dplyr::select(-c(
      screen_thickness,
      surficial_thickness,
      bedrock_thickness,
      litholog_present
    ))

  return(screens_completions)
}
