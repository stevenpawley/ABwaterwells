# Predefined query to extract a table of static water levels

Predefined query to extract a table of static water levels

## Usage

``` r
query_staticwater(wells, well_reports, pump_tests)
```

## Arguments

- wells:

  tibble of 'wells' data that has been metricated. The only columns that
  are required are c('wellid', 'gicwellid') but usually you would also
  want 'latitude' and 'longitude'.

- well_reports:

  tibble of 'wellreports' data that has been metricated. The required
  columns are c("wellid", "wellreportid").

- pump_tests:

  tibble of 'pumptests' data that has been metricated. Columns that are
  required in the pump tests download are c("wellreportid",
  "staticwaterlevel", "testdate")

## Value

tibble

## Examples

``` r
wells <-
  request_awwid("wells", select = "wellid,gicwellid,longitude,latitude") |>
  metricate()
#> Error in request_awwid("wells", select = "wellid,gicwellid,longitude,latitude"): could not find function "request_awwid"

well_reports <-
  request_awwid("wellreports", select = "wellid,wellreportid") |>
  metricate()
#> Error in request_awwid("wellreports", select = "wellid,wellreportid"): could not find function "request_awwid"

pumptests <-
  request_awwid(
    "pumptests",
    select = "wellreportid,staticwaterlevel,testdate"
) |>
  metricate()
#> Error in request_awwid("pumptests", select = "wellreportid,staticwaterlevel,testdate"): could not find function "request_awwid"

query_staticwater(wells, well_reports, pumptests) |>
  tidyr::drop_na(staticwaterlevel)
#> Error: object 'pumptests' not found
```
