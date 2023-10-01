#' AWWID table class
#'
#' @return a `tblAwwid` R6 class
#' @keywords internal
tblAwwid = R6::R6Class(
  classname = "tblAwwid",

  public = list(
    #' @field name name of the table
    name = NULL,

    #' @field data a data.table of the table data
    data = NULL,

    #' @field request the URL request that was made to obtain the table
    request = NULL,

    #' @description
    #' Create a new 'tblAwwid' object
    #' @param name name of the table
    #' @param x a data.table of the table data
    #' @param request the URL string that was used to request the table
    initialize = function(x, name, request) {
      self$name = name
      self$data = x
      self$request = request
    },

    #' @description
    #' Print method for the 'tblAwwid' class
    print = function() {
      cat("Table name:", self$name)
      cat("Request:", self$request)
      head(self$data)
      invisible(NULL)
    },

    #' @description
    #' Standardize a tblAwwid object by converting all measurement columns
    #' from imperial units to metric equivalents, and renaming some column
    #' names to avoid name collisions when joining tables
    metricate = function() {
      df = private$metricate_default(self$data)

      switch(
        self$name,
        analysisitems = private$metricate_analysisitems(df),
        boreholes = private$metricate_boreholes(df),
        wellcasinglogs = private$metricate_wellcasinglogs(df),
        chemicalanalysis = private$metricate_chemicalanalysis(df),
        drillingcompanies = private$metricate_drillingcompanies(df),
        elements = private$metricate_elements(df),
        lithologies = private$metricate_lithologies(df),
        otherseals = private$metricate_otherseals(df),
        pumptests = private$metricate_pumptests(df),
        pumptestitems = private$metricate_pumptestitems(df),
        screens = private$metricate_screens(df),
        wells = private$metricate_wells(df),
        wellmaterialslogs = private$metricate_wellmaterialslogs(df),
        welldecommissioningdetails = private$metricate_welldecommissioningdetails(df),
        wellreports = private$metricate_wellreports(df),
        perforations = private$metricate_perforations(df)
      )
    }
  ),

  private = list(
    ft_to_m = function(x) units::as_units(x * 0.3048, "m"),
    inch_to_cm = function(x) units::as_units(x * 2.54, "cm"),
    igpm_to_lpm = function(x) units::as_units(x * 4.54609, "L/m"),

    metricate_default = function(x) {
      # take copy
      x = data.table::copy(x)

      # set id columns to integer type
      cols = which(endsWith(names(x), "id"))
      for (j in cols) data.table::set(x, j = j, value = as.integer(x[[j]]))

      # datetime columns
      cols = grep("date|time", names(x))
      exclude = which(startsWith(names(x), "is"))
      exclude = c(exclude, which(endsWith(names(x), "flag")))
      cols = setdiff(cols, exclude)
      for (j in cols) {
        data.table::set(x, j = j, value = as.POSIXct(x[[j]], tz = "America/Edmonton"))
      }

      # flag columns
      cols = which(endsWith(names(x), "flag"))
      for (j in cols) data.table::set(x, j = j, value = as.logical(x[[j]]))
      return(x)
    },

    metricate_analysisitems = function(x) {
      cols = grep("value", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.numeric(x[[j]]))
      return(x)
    },

    metricate_boreholes = function(x) {
      # inches to cm columns
      cols = grep("diameter", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$inch_to_cm(x[[j]]))

      # feet to metres columns
      cols = grep("from|to", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      # rename columns to avoid collisions
      rename = c(boreholedepthfrom = "from", boreholedepthto = "to")
      data.table::setnames(x, rename, names(rename), skip_absent = TRUE)
      return(x)
    },

    metricate_wellcasinglogs = function(x, ...) {
      # feet to metres columns
      cols = grep("fromdepth|todepth", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      # inches to cm columns
      cols = grep("diameter", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$inch_to_cm(x[[j]]))

      # logical columns
      cols = grep("othermaterials", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.logical(x[[j]]))

      # rename columns
      rename = c(
        wellcasingdepthfrom = "fromdepth",
        wellcasingdepthto = "todepth",
        wellcasingdiameter = "diameter"
      )
      data.table::setnames(x, rename, names(rename), skip_absent = TRUE)
      return(x)
    },

    metricate_chemicalanalysis = function(x) {
      # ft to metres
      cols = grep("waterlevel", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      # set to character type
      cols = grep("aquifer|remarks", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.character(x[[j]]))
      return(x)
    },

    metricate_drillingcompanies = function(x) {
      # set lastwellidused to integer
      cols = grep("lastwellidused", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.integer(x[[j]]))
      return(x)
    },

    metricate_elements = function(x) {
      # set decimalplaces to integer
      cols = grep("decimalplaces", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.integer(x[[j]]))
      return(x)
    },

    metricate_lithologies = function(x) {
      # checks
      required_cols = c("depth", "wellreportid", "material", "description")
      stopifnot(all(required_cols %in% names(x)))

      # standardize columns and data types
      cols = grep("depth", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      cols = grep("waterbearing", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.logical(x[[j]]))

      # other changes to lithologs
      data.table::setnames(x, "depth", "lithdepthto")

      # sort each log by depth
      x = x[order(get("lithdepthto")), .SD, by = "wellreportid"]

      # calculate int_top_dep
      # awwid depth intervals represent 'to_depth', so the first row records
      # the bottom depth of the uppermost unit
      x[, c("lithdepthfrom") := data.table::shift(
        get("lithdepthto"), n = 1, type = "lag", fill = 0.0),
        by = "wellreportid"]

      cols = c(
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

      cols = intersect(cols, names(x))
      x = x[, .SD, .SDcols = cols]
      x[get("material") == "", c("material") := NA_character_]
      x[, c("material") := stringr::str_squish(get("material"))]
      x[get("description") == "", c("description") := NA_character_]
      return(x)
    },

    metricate_otherseals = function(x) {
      cols = grep("from|to|at", names(x))
      exclude = grep("date|time", names(x))
      cols = setdiff(cols, exclude)
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      rename = c(sealdepthfrom = "from", sealdepthto = "to",
                 sealotherdepth = "at")
      data.table::setnames(x, rename, names(rename), skip_absent = TRUE)
      return(x)
    },

    metricate_pumptests = function(x) {
      cols = grep("takenfromtopofcasing", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$inch_to_cm(x[[j]]))

      cols = grep("staticwaterlevel|endwaterlevel|removaldepthfrom", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      cols = grep("waterremovalrate", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$igpm_to_lpm(x[[j]]))

      cols = grep("reasonforshorttest", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.character(x[[j]]))
      return(x)
    },

    metricate_pumptestitems = function(x) {
      cols = grep("minutes", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.integer(x[[j]]))

      cols = grep("pumpingdepth|recoverydepth", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))
      return(x)
    },

    metricate_screens = function(x) {
      cols = grep("screentype", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.character(x[[j]]))

      cols = grep("minutes", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.integer(x[[j]]))

      cols = grep("from|to", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      cols = grep("slotsize|screeninsidediameter", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$inch_to_cm(x[[j]]))

      # rename
      rename = c(screendepthfrom = "from", screendepthto = "to",
                 screenslotsize = "slotsize")
      data.table::setnames(x, rename, names(rename), skip_absent = TRUE)
      return(x)
    },

    metricate_wells = function(x) {
      # numeric
      x = data.table::as.data.table(x)
      cols = grep("longitude|latitude|elevation", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.numeric(x[[j]]))

      # integer
      cols = c("distancenorth", "distancesouth", "distanceeast", "distancewest",
               "section", "township", "range", "meridian")
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = as.integer(x[[j]]))

      # character
      cols = grep("elevationobtained", names(x))
      for (j in cols) data.table::set(x, j = j, value = as.character(x[[j]]))

      # cleaning
      if ("goawelltagnumber" %in% names(x)) {
        x[get("goawelltagnumber") == "", c("goawelltagnumber") := NA_character_]
      }

      if ("boundaryfrom" %in% names(x)) {
        x[get("boundaryfrom") == "", c("boundaryfrom") := NA_character_]
      }

      if ("lot" %in% names(x)) {
        x[get("lot") == "", c("lot") := NA_character_]
      }

      if ("block" %in% names(x)) {
        x[get("block") == "", c("block") := NA_character_]
      }

      if ("plan" %in% names(x)) {
        x[get("plan") == "", c("plan") := NA_character_]
      }

      if ("additionaldescription" %in% names(x)) {
        x[get("additionaldescription") == "", c("additionaldescription") := NA_character_]
      }
      return(x)
    },

    metricate_wellmaterialslogs = function(x) {
      cols = grep("fromdepth|todepth", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))
      return(x)
    },

    metricate_welldecommissioningdetails = function(x) {
      cols = grep(
        "currentstaticwaterlevel|currentwelldepth|casingcutoffbelowgroundlevel",
        names(x)
      )
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      cols = c(
        "iswelldisinfectedpriortoplugging",
        "ispumpremoved",
        "iscasingcutoffbelowgroundlevel",
        "iswellreportcopygiventoowner",
        "iscertify",
        "iscompletedbyowner",
        "iscompletedbydriller"
      )
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = as.logical(x[[j]]))
      return(x)
    },

    metricate_wellreports = function(x) {
      # numeric
      cols = c(
        "plugmaterialamount",
        "annularsealamount",
        "packamount",
        "packgrainsize",
        "divertedwateramount"
      )
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = as.numeric(x[[j]]))

      # character
      cols = c(
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
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = as.character(x[[j]]))

      # feet to metres
      cols = c(
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
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      # inches to cm
      cols = c(
        "casingod",
        "casingthickness",
        "linerod",
        "screensizeod",
        "distancecasingground",
        "linerthickness"
      )
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = private$inch_to_cm(x[[j]]))

      # flow rate igpm tp m3ph
      cols = grep("artesianflowrate|recommendedrate", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$igpm_to_lpm(x[[j]]))

      # logical
      cols = c(
        "remedialaction",
        "welldisinfected",
        "otherlog",
        "issubmitted",
        "isvalidated",
        "drillingreportgiventoowner"
      )
      cols = grep(paste(cols, collapse = "|"), names(x))
      for (j in cols) data.table::set(x, j = j, value = as.logical(x[[j]]))
      return(data.table::as.data.table(x))
    },

    metricate_perforations = function(x) {
      # feet to metres
      cols = grep("from|to", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$ft_to_m(x[[j]]))

      # inches to cm
      cols = grep("diameter|interval|distancebetween", names(x))
      for (j in cols) data.table::set(x, j = j, value = private$inch_to_cm(x[[j]]))

      rename = c(
        perfdistance = "distancebetween",
        perfdepthfrom = "from",
        perfdepthto = "to",
        perfdiameter = "diameter",
        perfinterval = "interval"
      )
      data.table::setnames(x, rename, names(rename), skip_absent = TRUE)
      return(x)
    }
  )
)
