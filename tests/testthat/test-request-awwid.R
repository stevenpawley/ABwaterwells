test_that("AwwidQuery$request returns a TblAwwid object", {
  con <- AwwidQuery$new()
  tbl <- con$request("Wells", top = 10)
  expect_true(inherits(tbl, "TblAwwid"))
})

test_that("AwwidQuery$request metricate returns a data.table", {
  con <- AwwidQuery$new()

  tables <- c(
    "Wells", "AnalysisItems", "Boreholes", "MaterialOptions", "WellCasingLogs",
    "PlacementMethodOptions", "ChemicalAnalysis", "Drillers", "DrillingCompanies",
    "Elements", "GeophysicalLogs", "Lithologies", "OtherSeals", "Perforations",
    "PumpTests", "PumpTestItems", "Screens", "UnitOptions", "PlugMaterialOptions",
    "CasingStatus", "WellMaterialsLogs", "WellDecommissioningDetails",
    "WellDecommissioningReasons", "WellOwners", "WellReports"
  )

  for (tbl in tables) {
    df <- con$request(tbl, top = 10)$metricate()
    expect_s3_class(df, "data.table")
    expect_gt(nrow(df), 0, label = tbl)
  }
})

test_that("AwwidQuery$request caches results", {
  con <- AwwidQuery$new()
  con$request("Wells", top = 10)
  expect_false(is.null(con$.__enclos_env__$private$caching[[1]]))
})

test_that("AwwidQuery$request rejects unknown table names", {
  con <- AwwidQuery$new()
  expect_error(con$request("NotATable"), regexp = "`name` must be one of")
})

test_that("request row count matches @odata.count", {
  con <- AwwidQuery$new(cache = FALSE)
  name <- "materialoptions"
  count_url <- paste0(con$url, "/", name, "?$count=true&$top=0")
  resp_body <- httr2::request(count_url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expected_count <- as.integer(resp_body[["@odata.count"]])

  result <- con$request(name)
  expect_equal(nrow(result$data), expected_count)
})

test_that("chunked download matches single-request download and has no duplicates", {
  name <- "materialoptions"

  con_single <- AwwidQuery$new(cache = FALSE)
  direct <- con_single$request(name)$data

  # chunk_size = 2 forces many small chunks, exercising the parallel path
  con_chunked <- AwwidQuery$new(cache = FALSE, chunk_size = 2L)
  chunked <- con_chunked$request(name)$data

  key_col <- names(direct)[1]
  data.table::setkeyv(direct, key_col)
  data.table::setkeyv(chunked, key_col)

  expect_equal(nrow(chunked), nrow(direct))
  expect_false(anyDuplicated(chunked[[key_col]]) > 0)
  expect_equal(chunked[[key_col]], direct[[key_col]])
})
