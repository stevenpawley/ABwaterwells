# metric conversion functions ----
ft_to_m <- function(x) {
  units::as_units(x * 0.3048, "m")
}

inch_to_cm <- function(x) {
  units::as_units(x * 2.54, "cm")
}

igpm_to_lpm <- function(x) {
  units::as_units(x * 4.54609, "L/m")
}

# internal functions ----
standardize_awwid <- function(x) {
  # set id columns to integer type
  res <- x |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("id"), as.integer))

  # set datetime columns
  res <- res |>
    dplyr::mutate(dplyr::across(
      dplyr::contains(c("date", "time")) &
        !dplyr::starts_with("is") &
        !dplyr::ends_with("flag"),
      lubridate::as_datetime
    ))

  # flag columns
  res <- res |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("flag"), as.logical))

  return(res)
}

# metricate methods ----

#' Convert a tibble of water well related data into metric units
#'
#' @param x tibble of data derived from `request_awwid`
#' @param ... additional arguments that are currently unused
#'
#' @return a tibble
#' @export
metricate <- function(x, ...) {
  UseMethod("metricate", x)
}

#' @export
#' @exportS3Method metricate default
metricate.default <- function(x, ...) {
  standardize_awwid(x)
}

#' @export
#' @exportS3Method metricate analysisitems
metricate.analysisitems <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("value"), as.numeric))
}

#' @export
#' @exportS3Method metricate boreholes
metricate.boreholes <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("diameter"), inch_to_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("from", "to")), ft_to_m)) |>
    dplyr::rename(dplyr::any_of(c(
      boreholedepthfrom = "from",
      boreholedepthto = "to"
    )))
}

#' @export
#' @exportS3Method metricate wellcasinglogs
metricate.wellcasinglogs <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "fromdepth", "todepth"
    )), ft_to_m)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("diameter"), inch_to_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("othermaterials"), as.logical)) |>
    dplyr::rename(dplyr::any_of(
      c(
        wellcasingdepthfrom = "fromdepth",
        wellcasingdepthto = "todepth",
        wellcasingdiameter = "diameter"
      )
    ))
}

#' @export
#' @exportS3Method metricate chemicalanalysis
metricate.chemicalanalysis <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("waterlevel"), ft_to_m)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "aquifer", "remarks"
    )), as.character))
}

#' @export
#' @exportS3Method metricate drillingcompanies
metricate.drillingcompanies <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("lastwellidused"), as.integer))
}

#' @export
#' @exportS3Method metricate elements
metricate.elements <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("decimalplaces"), as.integer))
}

#' @export
#' @exportS3Method metricate lithologies
metricate.lithologies <- function(x, ...) {
  # some checks
  required_cols <-
    c("depth", "wellreportid", "material", "description")

  if (!all(required_cols %in% names(x))) {
    rlang::abort(glue::glue("need {required_cols} in data"))
  }

  # standardize columns and data types
  x <- x |>
    standardize_awwid() |>
    dplyr::mutate(depth = ft_to_m(.data$depth)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("waterbearing"), as.logical)) |>
    dplyr::rename(lithdepthto = "depth")

  # calculate int_top_dep
  # awwid depth intervals represent 'to_depth', so the first row records
  # the bottom depth of the uppermost unit
  # sort each log by depth
  x <- x |>
    dplyr::group_by(.data$wellreportid) |>
    dplyr::arrange(.data$lithdepthto, .by_group = TRUE) |>
    dplyr::ungroup()

  # calculate int_top_dep
  # awwid depth intervals represent 'to_depth', so the first row records
  # the bottom depth of the uppermost unit
  x <- x |>
    dplyr::group_by(.data$wellreportid) |>
    dplyr::mutate(lithdepthfrom = dplyr::lag(
      .data$lithdepthto,
      default = units::as_units(0, "m"))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::any_of(
      c(
        "lithologyid",
        "wellreportid",
        "lithdepthfrom",
        "lithdepthto",
        "material",
        "description",
        "waterbearing",
        "colour",
        "createtimestamp",
        "updatetimestamp"
      )
    )) |>
    dplyr::mutate(
      material = dplyr::na_if(.data$material, ""),
      description = dplyr::na_if(.data$description, "")
    )

  # set class
  class(x) <- c("lithologies", "awwid", class(x))

  return(x)
}

#' @export
#' @exportS3Method metricate otherseals
metricate.otherseals <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::contains(c("from", "to", "at")) &
                                  !dplyr::contains(c("date", "time")),
                                ft_to_m)) |>
    dplyr::rename(dplyr::any_of(
      c(
        sealdepthfrom = "from",
        sealdepthto = "to",
        sealotherdepth = "at"
      )
    ))
}

#' @export
#' @exportS3Method metricate pumptests
metricate.pumptests <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("takenfromtopofcasing"), inch_to_cm),
      dplyr::across(
        dplyr::any_of(c("staticwaterlevel", "endwaterlevel", "removaldepthfrom")),
        ft_to_m
      ),
      dplyr::across(dplyr::any_of("waterremovalrate"), igpm_to_lpm),
      dplyr::across(dplyr::any_of("reasonforshorttest"), as.character)
    )
}

#' @export
#' @exportS3Method metricate pumptestitems
metricate.pumptestitems <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::contains("minutes"), as.integer)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "pumpingdepth", "recoverydepth"
    )), ft_to_m))
}

#' @export
#' @exportS3Method metricate screens
metricate.screens <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("screentype"), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("minutes"), as.integer)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("from", "to")), ft_to_m)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "slotsize", "screeninsidediameter"
    )), inch_to_cm)) |>
    dplyr::rename(dplyr::any_of(
      c(
        screendepthfrom = "from",
        screendepthto = "to"
      )
    ))
}

#' @export
#' @exportS3Method metricate wells
metricate.wells <- function(x, ...) {
  x <- x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c("longitude", "latitude", "elevation")
    ), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "distancenorth",
        "distancesouth",
        "distanceeast",
        "distancewest",
        "section",
        "township",
        "range",
        "meridian"
      )
    ), as.integer)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "gpsobtained",
        "elevationobtained",
        "lot",
        "block",
        "plan",
        "additionaldescription",
        "goawelltagnumber",
        "boundaryfrom",
        "lsd"
      )
    ), as.character))

  # cleaning
  if ("goawelltagnumber" %in% names(x)) {
    x <- x |>
      dplyr::mutate(goawelltagnumber = dplyr::na_if(.data$goawelltagnumber, ""))
  }

  if ("boundaryfrom" %in% names(x)) {
    x <- x |>
      dplyr::mutate(boundaryfrom = dplyr::na_if(.data$boundaryfrom, ""))
  }

  if ("lot" %in% names(x)) {
    x <- x |>
      dplyr::mutate(lot = dplyr::na_if(.data$lot, ""))
  }

  if ("block" %in% names(x)) {
    x <- x |>
      dplyr::mutate(block = dplyr::na_if(.data$block, ""))
  }

  if ("plan" %in% names(x)) {
    x <- x |>
      dplyr::mutate(plan = dplyr::na_if(.data$plan, ""))
  }

  if ("additionaldescription" %in% names(x)) {
    x <- x |>
      dplyr::mutate(additionaldescription = dplyr::na_if(.data$additionaldescription, ""))
  }

  return(x)
}

#' @export
#' @exportS3Method metricate wellmaterialslogs
metricate.wellmaterialslogs <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::contains(c(
      "fromdepth", "todepth"
    )), ft_to_m)) |>
    dplyr::rename(dplyr::any_of(c(
      wellmaterialdepthfrom = "fromdepth",
      wellmaterialdepthto = "todepth"
    )))
}

#' @export
#' @exportS3Method metricate welldecommissioningdetails
metricate.welldecommissioningdetails <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "currentstaticwaterlevel",
        "currentwelldepth",
        "casingcutoffbelowgroundlevel"
      )
    ), ft_to_m)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "iswelldisinfectedpriortoplugging",
        "ispumpremoved",
        "iscasingcutoffbelowgroundlevel",
        "iswellreportcopygiventoowner",
        "iscertify",
        "iscompletedbyowner",
        "iscompletedbydriller"
      )
    ), as.logical))
}

#' @export
#' @exportS3Method metricate wellreports
metricate.wellreports <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "plugmaterialamount",
        "annularsealamount",
        "packamount",
        "packgrainsize",
        "divertedwateramount"
      )
    ), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "pluggedunits",
        "modeloutputrating",
        "divertedwatersource",
        "pumphorsepower",
        "pumpmodel",
        "pumptypeinstalled",
        "flowcontroldescription",
        "screenbottomfittings",
        "screentopfittings",
        "screenattachment",
        "screenmaterial",
        "annularsealunits",
        "pluggedunits",
        "plugmaterialtype",
        "otherwelluse",
        "flowcontroldescription",
        "createdby",
        "submittedby"
      )
    ), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "totaldepthdrilled",
        "finishedwelldepth",
        "casingbottom",
        "linertop",
        "linerbottom",
        "annularsealfrom",
        "annularsealto",
        "salinewaterdepth",
        "gasdepth",
        "recommendedintakedepth",
        "pumpinstalleddepth"
      )
    ), ft_to_m)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "casingod",
        "casingthickness",
        "linerod",
        "screensizeod",
        "distancecasingground",
        "linerthickness"
      )
    ), inch_to_cm)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c("artesianflowrate",
        "recommendedrate")
    ), igpm_to_lpm)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        "remedialaction",
        "welldisinfected",
        "otherlog",
        "issubmitted",
        "isvalidated",
        "drillingreportgiventoowner"
      )
    ), as.logical))
}

#' @export
#' @exportS3Method metricate perforations
metricate.perforations <- function(x, ...) {
  x |> standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("from", "to")), ft_to_m)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c("diameter", "interval", "distancebetween")
    ), inch_to_cm)) |>
    dplyr::rename(dplyr::any_of(
      c(
        perfdistance = "distancebetween",
        perfdepthfrom = "from",
        perfdepthto = "to",
        perfdiameter = "diameter",
        perfinterval = "interval"
      )
    ))
}
