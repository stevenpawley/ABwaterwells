
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ABwaterwells

<!-- badges: start -->
<!-- badges: end -->

The goal of **ABwaterwells** is to provide an R API to access water well
data in Alberta.

## Installation

You can install the development version of ABwaterwells from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stevenpawley/ABwaterwells")
```

## Example

A basic request of the ‘wells’ and ‘wellreports’ tables. Requests are
performed in chunks in parallel.

``` r
library(ABwaterwells)
library(future)
plan("multisession")

# create a connection
con = dbAwwid$new()

# request individual tables
wells = con$request(
  "wells", 
  select = c("gicwellid", "wellid", "longitude", "latitude"),
  filter = "gicwellid in (40000, 40001, 40002)"
)

reports = con$request(
  "wellreports", 
  select = c("wellid", "wellreportid", "totaldepthdrilled"),
  filter = "wellreportid in (40000, 40001, 40002)"
)

# the data from these objects is stored in the 'data' field
head(reports$data)
#> Key: <totaldepthdrilled>
#>    totaldepthdrilled wellid wellreportid
#>                <num>  <int>        <int>
#> 1:               149  40001        40001
#> 2:               195  40000        40000
#> 3:               268  40002        40002
```

To view the available tables:

``` r
con$tables
#>  [1] "analysisitems"              "boreholes"                 
#>  [3] "materialoptions"            "wellcasinglogs"            
#>  [5] "placementmethodoptions"     "chemicalanalysis"          
#>  [7] "drillers"                   "drillingcompanies"         
#>  [9] "elements"                   "geophysicallogs"           
#> [11] "lithologies"                "otherseals"                
#> [13] "perforations"               "pumptests"                 
#> [15] "pumptestitems"              "screens"                   
#> [17] "wells"                      "unitoptions"               
#> [19] "plugmaterialoptions"        "casingstatus"              
#> [21] "wellmaterialslogs"          "welldecommissioningdetails"
#> [23] "welldecommissioningreasons" "wellowners"                
#> [25] "wellreports"
```

Data in the [Alberta Water Well Information
Database](https://www.alberta.ca/alberta-water-well-information-database)
is stored in imperial units. The `metricate` method can be used to
automatically convert each table into metric units:

``` r
# the 'metricate' method automatically converts fields from each table into metric units
wells_df = wells$metricate()
reports_df = reports$metricate()

head(reports_df)
#>    totaldepthdrilled wellid wellreportid
#>              <units>  <int>        <int>
#> 1:       45.4152 [m]  40001        40001
#> 2:       59.4360 [m]  40000        40000
#> 3:       81.6864 [m]  40002        40002
```

To save on the number of requests, the **ABwaterwells** package
automatically caching the results of specific requests. This means if a
request is repeated, the data will be returned almost immediately:

``` r
con$request(
  "wells",
  select = c("gicwellid", "wellid", "longitude", "latitude"),
  filter = "gicwellid in (40000, 40001, 40002)"
)
#> Key: <gicwellid>
#>    gicwellid latitude longitude wellid
#>        <int>    <num>     <num>  <int>
#> 1:     40000 52.20076 -112.3637  40000
#> 2:     40001 52.22914 -112.7896  40001
#> 3:     40002 52.16615 -112.8882  40002
```

Several predefined queries that perform common processing tasks are also
included in the package. Caching is used in these queries so that
previous requests, for example, to the wells table, do not get performed
repeatedly:

``` r
lithologs = con$query_lithologs()
#> requesting `wells` table
#> Loading required namespace: future.apply
#> requesting `wellreports` table
#> requesting `lithologies` table
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■
#> Waiting 10s to retry ■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s to retry ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
screens = con$query_screens()
#> requesting `wells` table
#> requesting `wellreports` table
#> requesting `screens` table
#> requesting `perforations` table
waterlevel = con$query_water_level()
#> Requesting `wells` table
#> Requesting `pumptests` table
#> Requesting `wellreports` table
```

``` r
head(lithologs)
#>    gicwellid longitude latitude  gr_elev bh_depth location_type location_source
#>        <int>     <num>    <num>    <num>    <num>        <fctr>          <fctr>
#> 1:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database
#> 2:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database
#> 3:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database
#> 4:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database
#> 5:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database
#> 6:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database
#>    int_top_dep int_bot_dep  material material_desc colour waterbearing
#>          <num>       <num>    <char>        <char> <char>       <lgcl>
#> 1:      0.0000      0.3048   Topsoil          <NA>   <NA>        FALSE
#> 2:      0.3048      2.4384      Till          <NA>  Brown        FALSE
#> 3:      2.4384      4.2672      Clay          <NA>   Gray        FALSE
#> 4:      4.2672      7.3152     Shale          <NA>   Gray        FALSE
#> 5:      7.3152      8.2296 Sandstone Water Bearing   <NA>         TRUE
#> 6:      8.2296     13.7160     Shale          <NA>   Gray        FALSE
```
