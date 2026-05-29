
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ABwaterwells

<!-- badges: start -->

[![R-CMD-check](https://github.com/stevenpawley/ABwaterwells/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stevenpawley/ABwaterwells/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/stevenpawley/ABwaterwells/graph/badge.svg)](https://app.codecov.io/gh/stevenpawley/ABwaterwells)
<!-- badges: end -->

**ABwaterwells** provides an R interface to the [Alberta Water Well
Information
Database](https://www.alberta.ca/alberta-water-well-information-database)
(AWWID) via its OData web service. Large tables are downloaded in
parallel chunks and automatically converted from imperial to metric
units.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("stevenpawley/ABwaterwells")
```

## Getting started

`awwid_connect()` fetches the list of available tables once and returns
a connection object with one named field per table.

``` r
library(ABwaterwells)

con <- awwid_connect()
con
#> <awwid_connection> [37 tables]
#>   analysisitems, boreholes, materialoptions, wellcasinglogs,
#>   placementmethodoptions, chemicalanalysis, drillers,
#>   drillingcompanies, elements, geophysicallogs, lithologies,
#>   otherseals, perforations, pumptests, pumptestitems, screens, wells,
#>   unitoptions, plugmaterialoptions, casingstatus, wellmaterialslogs,
#>   welldecommissioningdetails, welldecommissioningreasons, wellowners,
#>   wellreports, analysisitem, borehole, lithology, otherseal,
#>   perforation, pumptestitem, pumptest, screen, wellcasinglog,
#>   welldecommissioningdetail, wellmaterialslog, wellreport
```

## Accessing tables by name

Pass any field from the connection object directly to `awwid_tbl()`.
Most IDEs will tab-complete the available table names, without the need
to type or remember them as characters.

``` r
wells <- awwid_tbl(
  con$wells,
  select = c("gicwellid", "wellid", "longitude", "latitude"),
  filter = "gicwellid gt 40000 and gicwellid lt 41000"
)

reports <- awwid_tbl(
  con$wellreports,
  select = c("wellid", "wellreportid", "totaldepthdrilled"),
  filter = "wellreportid gt 40000 and wellreportid lt 41000"
)

lithologies <- awwid_tbl(
  con$lithologies,
  filter = "wellreportid gt 40000 and wellreportid lt 41000"
)

head(lithologies)
#> # A tibble: 6 × 9
#>   lithologyid wellreportid depth waterbearing colour material description   
#>         <int>        <int> <dbl>        <int> <chr>  <chr>    <chr>         
#> 1    12729337        40914   219            0 Gray   Till     Stoney        
#> 2    12729338        40914   229            0 <NA>   Gravel   Dirty         
#> 3    12729339        40914   260            0 Gray   Till     Stoney        
#> 4    12729340        40914   274            0 <NA>   Sand     Coarse Grained
#> 5    12729341        40915    65            0 Gray   Till     Soft          
#> 6    12729342        40915    85            0 <NA>   Sand     Dirty         
#> # ℹ 2 more variables: createtimestamp <chr>, updatetimestamp <chr>
```

## Looping over multiple tables

For programmatic use such as iterating over a variable set of table
names, pass a character string to `awwid_tbl()` directly, or index into
the connection object with `[[`:

``` r
tables_to_download <- c("wells", "wellreports", "lithologies")

results <- lapply(tables_to_download, function(tbl) {
  awwid_tbl(con[[tbl]], top = 100)
})
names(results) <- tables_to_download
```

## Converting to metric units

Data in AWWID is stored in imperial units. Call `metricate()` to convert
a table automatically:

``` r
reports_df <- metricate(reports)
lithologies_df <- metricate(lithologies)

head(lithologies_df)
#> # A tibble: 6 × 10
#>   lithologyid wellreportid lithdepthfrom lithdepthto material description
#>         <int>        <int>           [m]         [m] <chr>    <chr>      
#> 1    12743989        40001         0           0.305 Topsoil  <NA>       
#> 2    12856372        40001         0.305       6.71  Till     <NA>       
#> 3    12743990        40001         6.71        7.62  Till     <NA>       
#> 4    12743991        40001         7.62       22.3   Shale    <NA>       
#> 5    12743992        40001        22.3        24.1   Shale    <NA>       
#> 6    12743993        40001        24.1        25.6   Shale    <NA>       
#> # ℹ 4 more variables: waterbearing <lgl>, colour <chr>, createtimestamp <dttm>,
#> #   updatetimestamp <dttm>
```

## Predefined queries

Several multi-table queries cover common workflows. Each accepts
already- downloaded, metricated tables as arguments, so you control when
downloads happen and can reuse data across calls.

``` r
lithologs <- query_lithologs(wells, reports_df, lithologies_df)
#> Warning: [extract] transforming vector data to the CRS of the raster
lithologs
#> # A tibble: 8,889 × 12
#>    gicwellid longitude latitude gr_elev bh_depth int_top_dep int_bot_dep
#>        <int>     <dbl>    <dbl>   <dbl>    <dbl>       <dbl>       <dbl>
#>  1     40001     -113.     52.2    845.     45.4       0           0.305
#>  2     40001     -113.     52.2    845.     45.4       0.305       6.71 
#>  3     40001     -113.     52.2    845.     45.4       6.71        7.62 
#>  4     40001     -113.     52.2    845.     45.4       7.62       22.3  
#>  5     40001     -113.     52.2    845.     45.4      22.3        24.1  
#>  6     40001     -113.     52.2    845.     45.4      24.1        25.6  
#>  7     40001     -113.     52.2    845.     45.4      25.6        31.7  
#>  8     40001     -113.     52.2    845.     45.4      31.7        32.3  
#>  9     40001     -113.     52.2    845.     45.4      32.3        42.1  
#> 10     40001     -113.     52.2    845.     45.4      42.1        44.5  
#> # ℹ 8,879 more rows
#> # ℹ 5 more variables: material <chr>, material_desc <chr>, colour <chr>,
#> #   waterbearing <lgl>, code <chr>
```

``` r
screens <- awwid_tbl(
  con$screens,
  filter = "wellreportid gt 40000 and wellreportid lt 41000"
)

perforations <- awwid_tbl(
  con$perforations,
  filter = "wellreportid gt 40000 and wellreportid lt 41000"
)

query_screens(
  wells = wells,
  wells_reports = reports_df,
  screens = metricate(screens),
  perforations = metricate(perforations),
  .aggregate = TRUE,
  .assumed_top = 5
)
```
