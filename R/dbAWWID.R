# dbAwwid R6 class ----

#' Create a connection to the AEPA AWWID web server
#'
#' @description
#' An R6 object that contains methods to request tables from the AWWID web server
#' using the OData protocol
#' @export
dbAwwid = R6::R6Class(
  classname = "dbAwwid",
  public = list(
    #' @field url The base URL
    url = "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata",

    #' @field tables A character vector of tables that are contained in AWWID.
    tables = NULL,

    #' @field cache Logical to indicate whether the results from the
    #'   queries/requests will be cached.
    cache = TRUE,

    #' @description
    #' Initialize a connection to the AEPA web server
    #' @param cache whether to internally cache the results of the requests.
    #'   This can increase performance for example, for queries where the wells
    #'   or well reports tables need to be repeatedly downloaded. The default is
    #'   TRUE.
    #' @return a R6 class.
    initialize = function(cache = TRUE) {
      self$tables = tolower(private$list_tables())
      self$cache = cache
    },

    #' @description
    #' Request a table from the web server with options to filter the data
    #' on the server using OData conventions
    #' @param name, name of the table to download.
    #' @param filter odata compatible query to filter the data before
    #'   downloading.
    #' @param select a character vector of columns to select.
    #' @param top integer, optionally return only the first specified number of
    #'   rows.
    #' @return a R6 class.
    request = function(name, filter = NULL, select = NULL, top = NULL) {
      # some checks
      if (!tolower(name) %in% tolower(self$tables)) {
        stop(glue::glue(
          "`name` must be one of {tables}",
          tables = paste(self$tables, collapse = ", ")
        ))
      }

      if (length(select) > 1) {
        select = sort(select)
        select = paste(select, collapse = ",")
      }

      # build request
      query = c()

      if (!is.null(filter)) {
        filter = URLencode(filter)
        query = c(query, glue::glue("$filter={filter}"))
      }

      if (!is.null(select)) {
        select = URLencode(select)
        query = c(query, glue::glue("$select={select}"))
      }

      # count number of records that request will generate
      query_count = c(query, "$count=true")
      query_count = paste(query_count, collapse = "&")
      query_count = paste0("?", query_count)
      resp = file.path(self$url, name, query_count) |>
        httr2::request() |>
        httr2::req_retry(max_tries = 10) |>
        httr2::req_perform()
      counts = as.integer(httr2::resp_body_json(resp)[["@odata.count"]])

      # check for previously cached results
      request_tag = private$add_query_options(name, query, top = top)
      if (!is.null(private$caching[[request_tag]])) {
        tbl = private$caching[[request_tag]]
        return(tbl)
      }

      # perform request in chunks of 10000 rows (REST max)
      if ((is.null(top) || top > 10000) & counts > 10000) {
        df = pbapply::pblapply(
          seq(0L, counts, by = 10000L),
          function(skip, name, query) {
            request_url = private$add_query_options(name, query, skip)
            private$get_query(request_url)
          },
          name = name,
          query = query,
          cl = "future"
        )
        request_url = private$add_query_options(name, query)
        df = data.table::rbindlist(df)
      } else {
        request_url = private$add_query_options(name, query, top = top)
        df = private$get_query(request_url)
        df = data.table::as.data.table(df)
      }

      data.table::setkeyv(df, names(df)[1])
      tbl = tblAwwid$new(name = name, x = df, request = request_url)

      if (self$cache) {
        request_tag = private$add_query_options(name, query, top = top)
        private$caching[[request_tag]] = tbl$clone()
      }

      return(tbl)
    },

    #' @description
    #' Predefined query to extract a table of lithologs data
    #' @details
    #' The 'query_lithologs' function combines well identifer and location
    #' information from the 'wells' and 'wellreports' tables, with interval
    #' descriptions from the 'lithologies' table.
    #' @param ext optional numeric vector specifying the rectangular bounding box
    #'   of wells to return. Must be specified as c(xmin, ymin, xmax, ymax).
    #' @return a data.table.
    query_lithologs = function(ext = NULL) {
      # request well data filtered by ext
      message("requesting `wells` table")
      awwid_wells = private$request_wells_geographic(ext)

      # request well reports filtered by wellid present in wells
      message("requesting `wellreports` table")
      report_cols = c("wellreportid", "wellid", "totaldepthdrilled")
      well_reports = self$request("wellreports", select = report_cols)$metricate()

      # request lithologies
      message("requesting `lithologies` table")
      lithologies_cols = c("wellreportid", "depth", "material", "description", "colour", "waterbearing")
      lithologies = self$request("lithologies", select = lithologies_cols)$metricate()

      # merge tables
      awwid_lithologies = merge(
        lithologies,
        well_reports,
        by = "wellreportid"
      )

      # join well table with lithologies
      awwid = merge(awwid_lithologies, awwid_wells, by = "wellid")
      todrop = c("wellreportid", "lithologyid", "wellid")
      todrop = c(todrop, names(awwid)[grep("time", names(awwid))])
      todrop = intersect(todrop, names(awwid))
      awwid = awwid[, .SD, .SDcols = -todrop]

      # rename columns
      awwid[get("material") == "", c("material") := NA_character_]
      awwid[get("description") == "", c("description") := NA_character_]

      rename = c(
        int_top_dep = "lithdepthfrom",
        int_bot_dep = "lithdepthto",
        bh_depth = "totaldepthdrilled",
        material_desc = "description"
      )
      data.table::setnames(awwid, rename, names(rename))
      awwid = units::drop_units(awwid)

      # remove logs containing intervals of zero thickness
      invalid_ids = awwid[get("int_top_dep") == get("int_bot_dep") | get("int_top_dep") < 0][["gicwellid"]]
      invalid_ids = unique(invalid_ids)
      awwid = awwid[!get("gicwellid") %in% invalid_ids]

      # check for distinct depths
      awwid = unique(awwid, by = c("gicwellid", "int_top_dep"))

      # add ground elevation
      awwid = private$add_ground_elevation(awwid)

      # add other standard columns
      awwid[, `:=`(
        location_type = as.factor("Well"),
        location_source = as.factor("Aenv database")
      )]

      # reorder columns
      col_order = c(
        "gicwellid",
        "longitude",
        "latitude",
        "gr_elev",
        "bh_depth",
        "location_type",
        "location_source",
        "int_top_dep",
        "int_bot_dep",
        "material",
        "material_desc",
        "colour",
        "waterbearing"
      )
      awwid = awwid[, .SD, .SDcols = col_order]

      return(awwid)
    },

    #' @description
    #' Predefined query to extract a table of combined screen and perforation
    #' depths
    #' @details
    #' The query downloads the screens and perforations tables. For wells that
    #' contain multiple screen/perforation depth intervals, the
    #' screen/perforation depths are aggregated by taking the top depth as the
    #' minimum depth of the combined screens, and the bottom depth as the
    #' maximum depth of the combined screens.
    #' @param ext optionally provide a character vector to specify the
    #' geographic extent of the downloaded data, in the order of
    #' c(xmin, ymin, xmax, ymax).
    #' @return return a data.table of depth to well screens.
    query_screens = function(ext = NULL) {
      # request well data filtered by ext
      message("requesting `wells` table")
      wells = private$request_wells_geographic(ext)

      # request well reports data
      message("requesting `wellreports` table")
      report_cols = c("wellreportid", "wellid", "totaldepthdrilled")
      reports = self$request("wellreports", select = report_cols)$metricate()
      reports[, c("totaldepthdrilled") := NULL]

      # request screens data
      message("requesting `screens` table")
      screen_cols = c("screenid", "wellreportid", "from", "to")
      screens = con$request("screens", select = screen_cols)$metricate()

      # request perforations data
      message("requesting `perforations` table")
      perf_cols = c("perforationid", "from", "to", "wellreportid")
      perfs = self$request("perforations", select = perf_cols)$metricate()

      # create a lookup table to relate gicwellid to the wellreportid
      linking = merge(reports, wells, by = "wellid")
      linking = linking[, .SD, .SDcols = c("wellid", "wellreportid", "gicwellid")]

      # combine the perforations and screens
      perfs_gicwellid = merge(perfs, linking, by = "wellreportid")
      data.table::setnames(
        perfs_gicwellid,
        c("perfdepthfrom", "perfdepthto"),
        c("screendepthfrom", "screendepthto")
      )

      screens_gicwellid = merge(screens, linking, by = "wellreportid")
      screens_perfs = rbind(screens_gicwellid, perfs_gicwellid, fill = TRUE)

      # aggregate the maximum depth range of screens/perfs for each well
      screens_avg = screens_perfs[
        order(screendepthfrom),
        .(wellreportid = data.table::first(get("wellreportid")),
          screendepthfrom = min(get("screendepthfrom"), na.rm = TRUE),
          screendepthto = max(get("screendepthto"), na.rm = TRUE)
        ),
        by = "gicwellid"
      ]

      # calculate the screen depth mid-point
      screens_avg[, c("screendepthmid") :=
                    get("screendepthfrom") + (get("screendepthto") - get("screendepthfrom")) / 2]
      return(screens_avg)
    },

    #' @description
    #' Predefined query to extract a table of static water depths from AWWID
    #' based on the pump tests table
    #' @details
    #' The query downloads the wells, well reports and pump tests tables. These
    #' are joined based on getting the linking the well reports to the wells
    #' table using the 'wellid' column. We do this because the well reports
    #' table also contains the 'gicwellid'. Then we join the pump tests table to
    #' the previous dataset based on the 'wellreportid'.
    #'
    #' To get the static water depth, we use the 'staticwaterlevel' column. For
    #' wells that have multiple pump tests, the method specified in 'keep' is
    #' used. The default is to retain only the newest test.
    #'
    #' @param ext optional numeric vector specifying the rectangular bounding box
    #'   of wells to return. Must be specified as c(xmin, ymin, xmax, ymax).
    #' @param keep character, method to use to aggregate wells that contain
    #'   multiple pump test measurements. One of c("newest", "average",
    #'   "maximum", "minimum").
    #' @return returns a data.table of static water levels.
    query_water_level = function(ext = NULL,
                                 keep = c("newest", "average", "maximum", "minimum")) {
      keep_method = match.arg(keep)

      # request required tables
      message("Requesting `wells` table")
      wells = private$request_wells_geographic(ext)
      data.table::setkeyv(wells, "wellid")

      message("Requesting `pumptests` table")
      pump_tests = self$request(
        name = "pumptests",
        select = c("wellreportid", "staticwaterlevel", "testdate")
      )
      pump_tests = pump_tests$metricate()
      data.table::setkeyv(pump_tests, "wellreportid")

      message("Requesting `wellreports` table")
      well_reports = self$request(
        name = "wellreports",
        select = c("wellid", "wellreportid", "totaldepthdrilled")
      )
      well_reports = well_reports$metricate()
      well_reports[, c("totaldepthdrilled") := NULL]
      data.table::setkeyv(well_reports, "wellreportid")

      # join the wells and well reports tables to get the gicwellid
      wellindex = well_reports[wells]

      # join the pump tests
      pumptests = wellindex[pump_tests]

      # aggregate multiple pump tests
      pumptests = pumptests[, .SD[order(get("testdate"))], by = "gicwellid"]

      if (keep_method == "newest") {
        pumptests_agg = pumptests[, data.table::first(.SD), by = "gicwellid"]

      } else if (keep_method %in% c("average", "maximum", "minimum")) {
        aggfunc = switch(
          keep_method,
          average = mean,
          maximum = max,
          minimum = min
        )

        pumptests_agg = pumptests[, .(
          staticwaterlevel = aggfunc(staticwaterlevel, na.rm = TRUE),
          wellid = data.table::first(wellid),
          wellreportid = data.table::first(wellreportid),
          longitude = data.table::first(longitude),
          latitude = data.table::first(latitude)
          ),
          by = "gicwellid"
        ]
      }

      return(pumptests_agg)
    },

    #' @description
    #' Clear the internal cache of previous requests
    #' @return NULL
    clear_cache = function() {
      self$private$caching = list()
    }
  ),

  private = list(
    list_tables = function() {
      metadata = self$url |>
        httr2::request() |>
        httr2::req_retry(max_tries = 10) |>
        httr2::req_perform()

      metadata = metadata |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON()

      return(metadata$value$name[1:25])
    },

    add_query_options = function(tablename, query, skip = NULL, top = NULL) {
      if (!is.null(skip)) {
        query = c(query, glue::glue("$skip={skip}"))
      }

      if (!is.null(top)) {
        query = c(query, glue::glue("$top={top}"))
      }

      query = paste(query, collapse = "&")
      query = paste0("?", query)

      file.path(self$url, tablename, query)
    },

    get_query = function(base_url) {
      # create and run request object
      resp = base_url |>
        httr2::request() |>
        httr2::req_cache(path = tempdir()) |>
        httr2::req_retry(
          max_tries = 10,
          is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 503),
          backoff = ~ 10
        )

      result = resp |>
        httr2::req_perform()

      # coerce output to json
      result = result |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON()

      # coerce to datatable
      df = data.table::as.data.table(result$value)
      data.table::setnames(df, tolower(names(df)))

      return(df)
    },

    add_ground_elevation = function(logs) {
      fp = system.file("extdata/dem.tif", package = "ABwaterwells")
      dem = terra::rast(fp)

      v = terra::vect(logs, geom = c("longitude", "latitude"), crs = "epsg:4326")
      v = terra::project(v, "epsg:3402")
      v$x = terra::crds(v)[, 1]
      v$y = terra::crds(v)[, 2]
      v = v |>
        as.data.frame() |>
        data.table::as.data.table()

      log_crds = v[, .SD, .SDcols = c("x", "y")]
      logs$gr_elev = terra::extract(dem, as.data.frame(log_crds), ID = FALSE)[[1]]
      return(logs)
    },

    request_wells_geographic = function(ext) {
      # request well data filtered by ext
      filter_query = NULL

      if (!is.null(ext)) {
        filter_query = glue::glue(
          "longitude ge {ext[1]} and longitude le {ext[3]} and latitude ge {ext[2]} and latitude le {ext[4]}"
        )
      }
      wells_cols = c("gicwellid", "wellid", "longitude", "latitude")
      self$request("wells", select = wells_cols, filter = filter_query)$metricate()
    },

    caching = list()
  )
)
