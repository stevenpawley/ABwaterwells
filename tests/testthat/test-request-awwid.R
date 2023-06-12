test_that("test awwid functions", {
  df <- request_awwid("Wells", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "wells")

  df <- request_awwid("AnalysisItems", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "analysisitems")

  df <- request_awwid("Boreholes", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "boreholes")

  df <- request_awwid("MaterialOptions", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "materialoptions")

  df <- request_awwid("WellCasingLogs", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "wellcasinglogs")

  df <- request_awwid("PlacementMethodOptions", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "placementmethodoptions")

  df <- request_awwid("ChemicalAnalysis", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "chemicalanalysis")

  df <- request_awwid("Drillers", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "drillers")

  df <- request_awwid("DrillingCompanies", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "drillingcompanies")

  df <- request_awwid("Elements", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "elements")

  df <- request_awwid("GeophysicalLogs", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "geophysicallogs")

  df <- request_awwid("Lithologies", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "lithologies")

  df <- request_awwid("OtherSeals", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "otherseals")

  df <- request_awwid("Perforations", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "perforations")

  df <- request_awwid("PumpTests", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "pumptests")

  df <- request_awwid("PumpTestItems", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "pumptestitems")

  df <- request_awwid("Screens", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "screens")

  df <- request_awwid("UnitOptions", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "unitoptions")

  df <- request_awwid("PlugMaterialOptions", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "plugmaterialoptions")

  df <- request_awwid("CasingStatus", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "casingstatus")

  df <- request_awwid("WellMaterialsLogs", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "wellmaterialslogs")

  df <- request_awwid("WellDecommissioningDetails", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "welldecommissioningdetails")

  df <- request_awwid("WellDecommissioningReasons", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "welldecommissioningreasons")

  df <- request_awwid("WellOwners", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "wellowners")

  df <- request_awwid("WellReports", top = 10) |>
    metricate()
  testthat::expect_s3_class(df, "wellreports")
})
