# AwwidQuery R6 class ----


#' Create a connection to the AEPA AWWID web server
#'
#' @description
#' An R6 object that contains methods to request tables from the AWWID web server
#' using the OData protocol
#' @export
AwwidQuery = R6::R6Class(
  classname = "AwwidQuery",
  public = list(
    #' @field url The base URL
    url = "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata",

    #' @field tables A character vector of tables that are contained in AWWID.
    tables = NULL,

    #' @field cache Logical to indicate whether the results from the
    #'   queries/requests will be cached.
    cache = TRUE,

    #' @field retry_max_tries Integer, maximum number of retries for failed requests.
    retry_max_tries = 10L,

    #' @field retry_backoff Integer, number of seconds to wait between retries.
    retry_backoff = 10L,

    #' @field progress purrr progress bar options
    progress = TRUE,

    #' @field chunk_size Integer, number of rows per chunk when paginating large tables.
    chunk_size = 10000L,

    #' @description
    #' Initialize a connection to the AEPA web server
    #' @param cache whether to internally cache the results of the requests.
    #'   This can increase performance for example, for queries where the wells
    #'   or well reports tables need to be repeatedly downloaded. The default is
    #'   TRUE.
    #' @param retry_max_tries Integer, maximum number of retries for failed requests.
    #' @param retry_backoff Integer, number of seconds to wait between retries.
    #' @param chunk_size Integer, number of rows per download chunk when paginating.
    #' @param .progress purrr progress bar options
    #' @return a R6 class.
    initialize = function(cache = TRUE, retry_max_tries = 10L, retry_backoff = 10L,
                          chunk_size = 10000L, .progress = TRUE) {
      self$tables = tolower(private$list_tables())
      self$cache = cache
      self$retry_max_tries = retry_max_tries
      self$retry_backoff = retry_backoff
      self$chunk_size = chunk_size
      self$progress = .progress
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
      name = tolower(name)

      # some checks
      if (!name %in% self$tables) {
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

      # check for previously cached results
      request_tag = private$add_query_options(name, query, top = top)
      if (!is.null(private$caching[[request_tag]])) {
        return(private$caching[[request_tag]])
      }

      # count number of records that request will generate ($top=0 avoids transferring rows)
      query_count = paste(c(query, "$count=true", "$top=0"), collapse = "&")
      count_url = paste0(paste(self$url, name, sep = "/"), "?", query_count)
      resp = httr2::request(count_url) |>
        httr2::req_retry(
          max_tries = self$retry_max_tries,
          is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 503),
          backoff = \(resp) self$retry_backoff
        ) |>
        httr2::req_perform(verbosity = 0)

      count_val = httr2::resp_body_json(resp)[["@odata.count"]]
      if (is.null(count_val)) {
        stop("Server did not return @odata.count; verify that the OData service supports $count.")
      }
      counts = as.integer(count_val)

      # perform request in parallel chunks, ordered by primary key for deterministic pagination
      if ((is.null(top) || top > self$chunk_size) & counts > self$chunk_size) {
        # discover primary key column (original casing) for $orderby
        pk_url = private$add_query_options(name, query, top = 1L)
        pk_resp = private$build_request(pk_url) |> httr2::req_perform(verbosity = 0)
        pk_col = names(jsonlite::fromJSON(httr2::resp_body_string(pk_resp))$value)[1]

        reqs = lapply(seq(0L, counts - 1L, by = self$chunk_size), function(skip) {
          url = private$add_query_options(
            name,
            c(query, glue::glue("$orderby={pk_col}")),
            skip = skip,
            top = self$chunk_size
          )
          private$build_request(url)
        })
        resps = httr2::req_perform_parallel(reqs, on_error = "continue", progress = self$progress)
        errors = !vapply(resps, inherits, logical(1), "httr2_response")
        if (any(errors)) {
          stop(sum(errors), " chunk(s) failed to download; try increasing retry attempts.")
        }
        df = data.table::rbindlist(lapply(resps, private$parse_odata_response))
        request_url = private$add_query_options(name, query)
      } else {
        request_url = private$add_query_options(name, query, top = top)
        df = private$get_query(request_url)
      }

      data.table::setkeyv(df, names(df)[1])
      tbl = TblAwwid$new(name = name, x = df, request = request_url)

      if (self$cache) {
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
      awwid[material == "", c("material") := NA_character_, env = list(material = "material")]
      awwid[description == "", c("description") := NA_character_, env = list(description = "description")]

      rename = c(
        int_top_dep = "lithdepthfrom",
        int_bot_dep = "lithdepthto",
        bh_depth = "totaldepthdrilled",
        material_desc = "description"
      )
      data.table::setnames(awwid, rename, names(rename))
      awwid = units::drop_units(awwid)

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
      reports[, "totaldepthdrilled" := NULL]

      # request screens data
      message("requesting `screens` table")
      screen_cols = c("screenid", "wellreportid", "from", "to")
      screens = self$request("screens", select = screen_cols)$metricate()

      # request perforations data
      message("requesting `perforations` table")
      perf_cols = c("perforationid", "from", "to", "wellreportid")
      perfs = self$request("perforations", select = perf_cols)$metricate()

      # relate gicwellid to the wellreportid
      wells_reports = merge(reports, wells, by = "wellid")
      selected_cols = c("wellid", "wellreportid", "gicwellid", "latitude", "longitude")
      wells_reports = wells_reports[, .SD, .SDcols = selected_cols]

      # combine the perforations and screens
      perfs_gicwellid = merge(perfs, wells_reports, by = "wellreportid")
      data.table::setnames(
        perfs_gicwellid,
        c("perfdepthfrom", "perfdepthto"),
        c("screendepthfrom", "screendepthto")
      )

      screens_gicwellid = merge(screens, wells_reports, by = "wellreportid")
      screens_perfs = rbind(screens_gicwellid, perfs_gicwellid, fill = TRUE)

      # aggregate the maximum depth range of screens/perfs for each well
      screens_avg = screens_perfs[
        order(screendepthfrom),
        .(wellreportid = data.table::first(wellreportid),
          screendepthfrom = min(screendepthfrom, na.rm = TRUE),
          screendepthto = max(screendepthto, na.rm = TRUE),
          latitude = data.table::first(latitude),
          longitude = data.table::first(longitude)
        ),
        by = "gicwellid",
        env = list(
          wellreportid = "wellreportid",
          screendepthfrom = "screendepthfrom",
          screendepthto = "screendepthto",
          latitude = "latitude",
          longitude = "longitude"
        )
      ]

      # calculate the screen depth mid-point
      screens_avg[,
        "screendepthmid" := screendepthfrom + (screendepthto - screendepthfrom) / 2,
        env = list(
          screendepthmid = "screendepthmid",
          screendepthfrom = "screendepthfrom",
          screendepthto = "screendepthto"
        )
      ]
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
                                 keep = c("all", "newest", "average", "maximum", "minimum")) {
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
      pumptests = pumptests[, .SD[order(testdate)], by = "gicwellid", env = list(testdate = "testdate")]

      if (keep_method == "newest") {
        pumptests_agg = pumptests[, data.table::last(.SD), by = "gicwellid"]

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
      } else {
        pumptests_agg = pumptests
      }

      return(pumptests_agg)
    },

    #' @description
    #' Clear the internal cache of previous requests
    #' @return NULL
    clear_cache = function() {
      private$caching = list()
    },

    #' @description Request the `wells` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    wells = function(filter = NULL, select = NULL, top = NULL) {
      self$request("wells", filter = filter, select = select, top = top)
    },

    #' @description Request the `wellreports` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    wellreports = function(filter = NULL, select = NULL, top = NULL) {
      self$request("wellreports", filter = filter, select = select, top = top)
    },

    #' @description Request the `lithologies` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    lithologies = function(filter = NULL, select = NULL, top = NULL) {
      self$request("lithologies", filter = filter, select = select, top = top)
    },

    #' @description Request the `boreholes` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    boreholes = function(filter = NULL, select = NULL, top = NULL) {
      self$request("boreholes", filter = filter, select = select, top = top)
    },

    #' @description Request the `screens` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    screens = function(filter = NULL, select = NULL, top = NULL) {
      self$request("screens", filter = filter, select = select, top = top)
    },

    #' @description Request the `perforations` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    perforations = function(filter = NULL, select = NULL, top = NULL) {
      self$request("perforations", filter = filter, select = select, top = top)
    },

    #' @description Request the `pumptests` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    pumptests = function(filter = NULL, select = NULL, top = NULL) {
      self$request("pumptests", filter = filter, select = select, top = top)
    },

    #' @description Request the `pumptestitems` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    pumptestitems = function(filter = NULL, select = NULL, top = NULL) {
      self$request("pumptestitems", filter = filter, select = select, top = top)
    },

    #' @description Request the `chemicalanalysis` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    chemicalanalysis = function(filter = NULL, select = NULL, top = NULL) {
      self$request("chemicalanalysis", filter = filter, select = select, top = top)
    },

    #' @description Request the `analysisitems` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    analysisitems = function(filter = NULL, select = NULL, top = NULL) {
      self$request("analysisitems", filter = filter, select = select, top = top)
    },

    #' @description Request the `elements` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    elements = function(filter = NULL, select = NULL, top = NULL) {
      self$request("elements", filter = filter, select = select, top = top)
    },

    #' @description Request the `wellcasinglogs` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    wellcasinglogs = function(filter = NULL, select = NULL, top = NULL) {
      self$request("wellcasinglogs", filter = filter, select = select, top = top)
    },

    #' @description Request the `wellmaterialslogs` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    wellmaterialslogs = function(filter = NULL, select = NULL, top = NULL) {
      self$request("wellmaterialslogs", filter = filter, select = select, top = top)
    },

    #' @description Request the `otherseals` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    otherseals = function(filter = NULL, select = NULL, top = NULL) {
      self$request("otherseals", filter = filter, select = select, top = top)
    },

    #' @description Request the `geophysicallogs` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    geophysicallogs = function(filter = NULL, select = NULL, top = NULL) {
      self$request("geophysicallogs", filter = filter, select = select, top = top)
    },

    #' @description Request the `drillers` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    drillers = function(filter = NULL, select = NULL, top = NULL) {
      self$request("drillers", filter = filter, select = select, top = top)
    },

    #' @description Request the `drillingcompanies` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    drillingcompanies = function(filter = NULL, select = NULL, top = NULL) {
      self$request("drillingcompanies", filter = filter, select = select, top = top)
    },

    #' @description Request the `wellowners` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    wellowners = function(filter = NULL, select = NULL, top = NULL) {
      self$request("wellowners", filter = filter, select = select, top = top)
    },

    #' @description Request the `welldecommissioningdetails` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    welldecommissioningdetails = function(filter = NULL, select = NULL, top = NULL) {
      self$request("welldecommissioningdetails", filter = filter, select = select, top = top)
    },

    #' @description Request the `welldecommissioningreasons` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    welldecommissioningreasons = function(filter = NULL, select = NULL, top = NULL) {
      self$request("welldecommissioningreasons", filter = filter, select = select, top = top)
    },

    #' @description Request the `materialoptions` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    materialoptions = function(filter = NULL, select = NULL, top = NULL) {
      self$request("materialoptions", filter = filter, select = select, top = top)
    },

    #' @description Request the `unitoptions` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    unitoptions = function(filter = NULL, select = NULL, top = NULL) {
      self$request("unitoptions", filter = filter, select = select, top = top)
    },

    #' @description Request the `plugmaterialoptions` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    plugmaterialoptions = function(filter = NULL, select = NULL, top = NULL) {
      self$request("plugmaterialoptions", filter = filter, select = select, top = top)
    },

    #' @description Request the `placementmethodoptions` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    placementmethodoptions = function(filter = NULL, select = NULL, top = NULL) {
      self$request("placementmethodoptions", filter = filter, select = select, top = top)
    },

    #' @description Request the `casingstatus` table.
    #' @param filter OData filter expression.
    #' @param select character vector of column names to select.
    #' @param top integer, return only the first n rows.
    #' @return a TblAwwid object.
    casingstatus = function(filter = NULL, select = NULL, top = NULL) {
      self$request("casingstatus", filter = filter, select = select, top = top)
    }
  ),

  private = list(
    list_tables = function() {
      metadata = self$url |>
        httr2::request() |>
        httr2::req_retry(
          max_tries = self$retry_max_tries,
          backoff = \(resp) self$retry_backoff
        ) |>
        httr2::req_perform()

      metadata = metadata |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON()

      return(metadata$value$name)
    },

    add_query_options = function(tablename, query, skip = NULL, top = NULL) {
      if (!is.null(skip)) query = c(query, glue::glue("$skip={skip}"))
      if (!is.null(top)) query = c(query, glue::glue("$top={top}"))

      query = paste(query, collapse = "&")
      url = paste(self$url, tablename, sep = "/")
      if (nzchar(query)) url = paste0(url, "?", query)
      url
    },

    build_request = function(url) {
      httr2::request(url) |>
        httr2::req_cache(path = tempdir()) |>
        httr2::req_retry(
          max_tries = self$retry_max_tries,
          is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 503),
          backoff = \(resp) self$retry_backoff
        )
    },

    parse_odata_response = function(resp) {
      result = httr2::resp_body_string(resp) |> jsonlite::fromJSON()
      dt = data.table::as.data.table(result$value)
      data.table::setnames(dt, tolower(names(dt)))
      dt
    },

    get_query = function(url) {
      private$build_request(url) |>
        httr2::req_perform(verbosity = 0) |>
        private$parse_odata_response()
    },

    add_ground_elevation = function(logs) {
      fp = system.file("extdata/dem.tif", package = "ABwaterwells")
      dem = terra::rast(fp)

      v = terra::vect(logs, geom = c("longitude", "latitude"), crs = "epsg:4326")
      v = terra::project(v, "epsg:3402")
      v$x = terra::crds(v)[, 1]
      v$y = terra::crds(v)[, 2]
      v = as.data.frame(v) |>
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
