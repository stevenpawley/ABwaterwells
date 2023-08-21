allocate <- function(lithologs, screens, model) {
  # prepare lithologs for ml prediction model
  lithologs_sf <- lithologs |>
    tidyr::drop_na(c("latitude", "longitude")) |>
    dplyr::group_by(.data$gicwellid) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  lithologs_sf$x <- sf::st_coordinates(lithologs_sf)[, 1]
  lithologs_sf$y <- sf::st_coordinates(lithologs_sf)[, 2]
  lithologs_tm <- lithologs_sf |>
    sf::st_drop_geometry() |>
    dplyr::group_by(gicwellid)

  # model prediction
  predicted_intervals <- predict(model, lithologs_tm)
  predicted_intervals <- dplyr::bind_cols(predicted_intervals, lithologs_tm)

  predicted_tops <-
    pick_bedrock_last(predicted_intervals) |>
    dplyr::rename(bedrock_dep = ".bedrock_dep")

  predicted_logs <- dplyr::left_join(predicted_intervals, predicted_tops)

  predicted_picks <- predicted_logs |>
    dplyr::group_by(.data$gicwellid) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select("gicwellid", "bedrock_dep") |>
    dplyr::mutate(litholog_present = !is.na(.data$bedrock_dep))

  # join predicted picks with the screens
  screens_picked <- dplyr::left_join(screens, predicted_picks, by = "gicwellid")

  screens_picked <- dplyr::left_join(
    screens_picked,
    wells |> dplyr::select("gicwellid", "latitude", "longitude"),
    by = "gicwellid"
  )

  # extract missing values from interpolated surface
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

  # allocated each completion interval to surficial or bedrock
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
