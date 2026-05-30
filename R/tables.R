# metric conversion functions ----
ft_to_m <- function(x) {
  units::as_units(x * 0.3048, "m")
}

inch_to_cm <- function(x) {
  units::as_units(x * 2.54, "cm")
}

igpm_to_lpm <- function(x) {
  units::as_units(x * 4.54609, "L/min")
}

# column rename mapping: new_snake_name = "old_api_name" ----
.awwid_col_rename <- c(
  # IDs
  well_id = "wellid",
  gic_well_id = "gicwellid",
  well_report_id = "wellreportid",
  drilling_company_id = "drillingcompanyid",
  lithology_id = "lithologyid",
  element_id = "elementid",
  chemical_analysis_id = "chemicalanalysisid",
  well_owner_id = "wellownerid",
  driller_id = "drillerid",
  driller_instance_id = "drillerinstanceid",
  pump_test_id = "pumptestid",
  pump_test_item_id = "pumptestitemid",
  screen_id = "screenid",
  perforation_id = "perforationid",
  borehole_id = "boreholeid",
  well_casing_log_id = "wellcasinglogid",
  geophysical_log_id = "geophysicallogid",
  other_seal_id = "othersealid",
  plug_material_option_id = "plugmaterialoptionid",
  material_option_id = "materialoptionid",
  unit_option_id = "unitoptionid",
  placement_method_option_id = "placementmethodoptionid",
  casing_status_id = "casingstatusid",
  well_decommissioning_detail_id = "welldecommissioningdetailid",
  well_decommissioning_reason_id = "welldecommissioningreasonid",
  well_material_log_id = "wellmateriallogid",
  existing_well_id = "existingwellid",
  starting_well_id = "startingwellid",
  ending_well_id = "endingwellid",
  last_well_id_used = "lastwellidused",
  drilling_company_well_id = "drillingcompanywellid",
  gwris_id = "gwrisid",
  # timestamps
  create_timestamp = "createtimestamp",
  update_timestamp = "updatetimestamp",
  # Wells
  gps_obtained = "gpsobtained",
  elevation_obtained = "elevationobtained",
  distance_north = "distancenorth",
  distance_south = "distancesouth",
  distance_east = "distanceeast",
  distance_west = "distancewest",
  additional_description = "additionaldescription",
  goa_well_tag_number = "goawelltagnumber",
  boundary_from = "boundaryfrom",
  # DrillingCompanies
  company_name = "companyname",
  street_address = "streetaddress",
  postal_code = "postalcode",
  # Drillers
  last_name = "lastname",
  middle_initial = "middleinitial",
  first_name = "firstname",
  journeyman_number = "journeymannumber",
  is_active_flag = "isactiveflag",
  # GeophysicalLogs
  log_type = "logtype",
  log_taken_flag = "logtakenflag",
  sent_to_aenv_flag = "senttoaenvflag",
  # Lithologies (post-metricate names)
  lith_depth_from = "lithdepthfrom",
  lith_depth_to = "lithdepthto",
  water_bearing = "waterbearing",
  # ChemicalAnalysis
  sample_number = "samplenumber",
  sample_date = "sampledate",
  analysis_date = "analysisdate",
  water_level = "waterlevel",
  # Elements
  element_name = "elementname",
  element_symbol = "elementsymbol",
  decimal_places = "decimalplaces",
  unit_of_measure = "unitofmeasure",
  # Boreholes (post-metricate names)
  borehole_depth_from = "boreholedepthfrom",
  borehole_depth_to = "boreholedepthto",
  # WellCasingLogs (post-metricate names)
  well_casing_depth_from = "wellcasingdepthfrom",
  well_casing_depth_to = "wellcasingdepthto",
  well_casing_diameter = "wellcasingdiameter",
  other_materials = "othermaterials",
  # OtherSeals (post-metricate names)
  other_seal_type = "othersealtype",
  seal_depth_from = "sealdepthfrom",
  seal_depth_to = "sealdepthto",
  seal_other_depth = "sealotherdepth",
  # Perforations (post-metricate names)
  perf_depth_from = "perfdepthfrom",
  perf_depth_to = "perfdepthto",
  perf_diameter = "perfdiameter",
  perf_interval = "perfinterval",
  perf_distance = "perfdistance",
  # PumpTests
  test_date = "testdate",
  start_time = "starttime",
  taken_from_top_of_casing = "takenfromtopofcasing",
  static_water_level = "staticwaterlevel",
  end_water_level = "endwaterlevel",
  water_removal_type = "waterremovaltype",
  water_removal_rate = "waterremovalrate",
  removal_depth_from = "removaldepthfrom",
  reason_for_short_test = "reasonforshorttest",
  # PumpTestItems
  pumping_depth = "pumpingdepth",
  recovery_depth = "recoverydepth",
  # Screens (post-metricate names)
  screen_depth_from = "screendepthfrom",
  screen_depth_to = "screendepthto",
  screen_inside_diameter = "screeninsidediameter",
  slot_size = "slotsize",
  screen_type = "screentype",
  screen_depth_mid = "screendepthmid",
  # WellMaterialsLogs (post-metricate names)
  well_material_depth_from = "wellmaterialdepthfrom",
  well_material_depth_to = "wellmaterialdepthto",
  # WellDecommissioningDetails
  work_complete_date = "workcompletedate",
  current_static_water_level = "currentstaticwaterlevel",
  current_well_depth = "currentwelldepth",
  other_reasons = "otherreasons",
  is_well_disinfected_prior_to_plugging = "iswelldisinfectedpriortoplugging",
  is_pump_removed = "ispumpremoved",
  pump_not_removed_explanation = "pumpnotremovedexplanation",
  is_casing_cutoff_below_ground_level = "iscasingcutoffbelowgroundlevel",
  casing_cutoff_below_ground_level = "casingcutoffbelowgroundlevel",
  casing_not_cutoff_explanation = "casingnotcutoffexplanation",
  additional_comments = "additionalcomments",
  is_well_report_copy_given_to_owner = "iswellreportcopygiventoowner",
  is_certify = "iscertify",
  person_responsible = "personresponsible",
  is_completed_by_owner = "iscompletedbyowner",
  is_completed_by_driller = "iscompletedbydriller",
  certification_notice = "certificationnotice",
  date_report_received = "datereportreceived",
  # WellOwners
  owner_name = "ownername",
  # WellReports
  date_received = "datereceived",
  drilling_method = "drillingmethod",
  type_of_work = "typeofwork",
  plug_date = "plugdate",
  plug_material_type = "plugmaterialtype",
  plug_material_amount = "plugmaterialamount",
  plug_units = "plugunits",
  plugged_units = "pluggedunits",
  well_use = "welluse",
  other_well_use = "otherwelluse",
  total_depth_drilled = "totaldepthdrilled",
  finished_well_depth = "finishedwelldepth",
  drilling_start_date = "drillingstartdate",
  drilling_end_date = "drillingenddate",
  casing_material = "casingmaterial",
  casing_od = "casingod",
  casing_thickness = "casingthickness",
  casing_bottom = "casingbottom",
  liner_material = "linermaterial",
  liner_od = "linerod",
  liner_thickness = "linerthickness",
  liner_top = "linertop",
  liner_bottom = "linerbottom",
  perforation_by = "perforationby",
  annular_seal_material = "annularsealmaterial",
  annular_seal_from = "annularsealfrom",
  annular_seal_to = "annularsealto",
  annular_seal_amount = "annularsealamount",
  annular_seal_units = "annularsealunits",
  screen_material = "screenmaterial",
  screen_size_od = "screensizeod",
  screen_attachment = "screenattachment",
  screen_top_fittings = "screentopfittings",
  screen_bottom_fittings = "screenbottomfittings",
  pack_type = "packtype",
  pack_grain_size = "packgrainsize",
  pack_amount = "packamount",
  pack_units = "packunits",
  location_verification_method = "locationverificationmethod",
  distance_casing_ground = "distancecasingground",
  artesian_flow_flag = "artesianflowflag",
  artesian_flow_rate = "artesianflowrate",
  encounter_saline_water_flag = "encountersalinewaterflag",
  saline_water_depth = "salinewaterdepth",
  gas_depth = "gasdepth",
  encounter_gas_flag = "encountergasflag",
  remedial_action = "remedialaction",
  flow_control_installed_flag = "flowcontrolinstalledflag",
  flow_control_description = "flowcontroldescription",
  recommended_rate = "recommendedrate",
  recommended_intake_depth = "recommendedintakedepth",
  pump_installed_flag = "pumpinstalledflag",
  pump_installed_depth = "pumpinstalleddepth",
  pump_type_installed = "pumptypeinstalled",
  pump_model = "pumpmodel",
  pump_horsepower = "pumphorsepower",
  well_disinfected = "welldisinfected",
  other_log = "otherlog",
  potability_sample_taken_flag = "potabilitysampletakenflag",
  potability_sample_sent_to_aenv_flag = "potabilitysamplesenttoaenvflag",
  diverted_water_source = "divertedwatersource",
  diverted_water_amount = "divertedwateramount",
  diversion_datetime = "diversiondatetime",
  created_by = "createdby",
  is_submitted = "issubmitted",
  submitted_by = "submittedby",
  is_validated = "isvalidated",
  approval_holder_signature_date = "approvalholdersignaturedate",
  drilling_report_given_to_owner = "drillingreportgiventoowner",
  model_output_rating = "modeloutputrating"
)

#' Rename AWWID columns to snake_case
#'
#' @description
#' Renames the concatenated-lowercase column names returned by [metricate()]
#' (e.g. `gicwellid`, `totaldepthdrilled`) to readable snake_case equivalents
#' (e.g. `gic_well_id`, `total_depth_drilled`). Columns not in the rename
#' table are left unchanged.
#'
#' @param x a tibble returned by [metricate()].
#'
#' @return a tibble with renamed columns.
#' @export
awwid_clean_names <- function(x) {
  dplyr::rename(x, dplyr::any_of(.awwid_col_rename))
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
#' @param x tibble returned by [awwid_tbl()]
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
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("value"), as.numeric))
}

#' @export
#' @exportS3Method metricate boreholes
metricate.boreholes <- function(x, ...) {
  x |>
    standardize_awwid() |>
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
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "fromdepth",
        "todepth"
      )),
      ft_to_m
    )) |>
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
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("waterlevel"), ft_to_m)) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "aquifer",
        "remarks"
      )),
      as.character
    ))
}

#' @export
#' @exportS3Method metricate drillingcompanies
metricate.drillingcompanies <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("lastwellidused"), as.integer))
}

#' @export
#' @exportS3Method metricate elements
metricate.elements <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("decimalplaces"), as.integer))
}

#' @export
#' @exportS3Method metricate lithologies
metricate.lithologies <- function(x, ...) {
  # some checks
  required_cols <-
    c("depth", "wellreportid", "material", "description")

  if (!all(required_cols %in% names(x))) {
    abort(glue("need {required_cols} in data"))
  }

  x <- x |>
    standardize_awwid() |>
    dplyr::mutate(
      depth = ft_to_m(.data$depth),
      dplyr::across(dplyr::any_of("waterbearing"), as.logical)
    ) |>
    dplyr::rename(lithdepthto = "depth") |>
    dplyr::group_by(.data$wellreportid) |>
    dplyr::arrange(.data$lithdepthto, .by_group = TRUE) |>
    dplyr::mutate(
      lithdepthfrom = dplyr::lag(
        .data$lithdepthto,
        default = units::as_units(0, "m")
      )
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
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(
      dplyr::contains(c("from", "to", "at")) &
        !dplyr::contains(c("date", "time")),
      ft_to_m
    )) |>
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
        dplyr::any_of(c(
          "staticwaterlevel",
          "endwaterlevel",
          "removaldepthfrom"
        )),
        ft_to_m
      ),
      dplyr::across(dplyr::any_of("waterremovalrate"), igpm_to_lpm),
      dplyr::across(dplyr::any_of("reasonforshorttest"), as.character)
    )
}

#' @export
#' @exportS3Method metricate pumptestitems
metricate.pumptestitems <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::contains("minutes"), as.integer)) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "pumpingdepth",
        "recoverydepth"
      )),
      ft_to_m
    ))
}

#' @export
#' @exportS3Method metricate screens
metricate.screens <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of("screentype"), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("minutes"), as.integer)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("from", "to")), ft_to_m)) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "slotsize",
        "screeninsidediameter"
      )),
      inch_to_cm
    )) |>
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
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c("longitude", "latitude", "elevation")
      ),
      as.numeric
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
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
      ),
      as.integer
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
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
      ),
      as.character
    ))

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
      dplyr::mutate(
        additionaldescription = dplyr::na_if(.data$additionaldescription, "")
      )
  }

  return(x)
}

#' @export
#' @exportS3Method metricate wellmaterialslogs
metricate.wellmaterialslogs <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(
      dplyr::contains(c(
        "fromdepth",
        "todepth"
      )),
      ft_to_m
    )) |>
    dplyr::rename(dplyr::any_of(c(
      wellmaterialdepthfrom = "fromdepth",
      wellmaterialdepthto = "todepth"
    )))
}

#' @export
#' @exportS3Method metricate welldecommissioningdetails
metricate.welldecommissioningdetails <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c(
          "currentstaticwaterlevel",
          "currentwelldepth",
          "casingcutoffbelowgroundlevel"
        )
      ),
      ft_to_m
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c(
          "iswelldisinfectedpriortoplugging",
          "ispumpremoved",
          "iscasingcutoffbelowgroundlevel",
          "iswellreportcopygiventoowner",
          "iscertify",
          "iscompletedbyowner",
          "iscompletedbydriller"
        )
      ),
      as.logical
    ))
}

#' @export
#' @exportS3Method metricate wellreports
metricate.wellreports <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c(
          "plugmaterialamount",
          "annularsealamount",
          "packamount",
          "packgrainsize",
          "divertedwateramount"
        )
      ),
      as.numeric
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
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
      ),
      as.character
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
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
      ),
      ft_to_m
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c(
          "casingod",
          "casingthickness",
          "linerod",
          "screensizeod",
          "distancecasingground",
          "linerthickness"
        )
      ),
      inch_to_cm
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c("artesianflowrate", "recommendedrate")
      ),
      igpm_to_lpm
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c(
          "remedialaction",
          "welldisinfected",
          "otherlog",
          "issubmitted",
          "isvalidated",
          "drillingreportgiventoowner"
        )
      ),
      as.logical
    ))
}

#' @export
#' @exportS3Method metricate perforations
metricate.perforations <- function(x, ...) {
  x |>
    standardize_awwid() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("from", "to")), ft_to_m)) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(
        c("diameter", "interval", "distancebetween")
      ),
      inch_to_cm
    )) |>
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
