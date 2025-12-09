---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# ABwaterwells

<!-- badges: start -->

<!-- badges: end -->

The goal of **ABwaterwells** is to provide an R API to access water well data in Alberta.

## Installation

You can install the development version of ABwaterwells from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stevenpawley/ABwaterwells")
```

## Example

A basic request of the 'wells' and 'wellreports' tables. Requests are performed in chunks in parallel.


``` r
library(ABwaterwells)
future::plan("multisession")

# create a connection
con = AwwidQuery$new()
#> Error: object 'AwwidQuery' not found

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
#>  [1] "analysisitems"              "boreholes"                  "materialoptions"            "wellcasinglogs"            
#>  [5] "placementmethodoptions"     "chemicalanalysis"           "drillers"                   "drillingcompanies"         
#>  [9] "elements"                   "geophysicallogs"            "lithologies"                "otherseals"                
#> [13] "perforations"               "pumptests"                  "pumptestitems"              "screens"                   
#> [17] "wells"                      "unitoptions"                "plugmaterialoptions"        "casingstatus"              
#> [21] "wellmaterialslogs"          "welldecommissioningdetails" "welldecommissioningreasons" "wellowners"                
#> [25] "wellreports"
```

Data in the [Alberta Water Well Information Database](https://www.alberta.ca/alberta-water-well-information-database) is stored in imperial units. The `metricate` method can be used to automatically convert each table into metric units:


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

To save on the number of requests, the **ABwaterwells** package automatically caching the results of specific requests. This means if a request is repeated, the data will be returned almost immediately:


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

Several predefined queries that perform common processing tasks are also included in the package. Caching is used in these queries so that previous requests, for example, to the wells table, do not get performed repeatedly:


``` r
lithologs = con$query_lithologs()
#> requesting `wells` table
#> requesting `wellreports` table
#> requesting `lithologies` table
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■                            
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■                            
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■
#> Waiting 10s for retry backoff ■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 10s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
screens = con$query_screens()
#> requesting `wells` table
#> requesting `wellreports` table
#> requesting `screens` table
#> requesting `perforations` table
#> Error in con$query_screens(): object 'linking' not found
waterlevel = con$query_water_level()
#> Requesting `wells` table
#> Requesting `pumptests` table
#> Requesting `wellreports` table
#> Processed 55802 groups out of 253484. 22% done. Time elapsed: 3s. ETA: 10s.Processed 73205 groups out of 253484. 29% done. Time elapsed: 4s. ETA: 9s.Processed 93152 groups out of 253484. 37% done. Time elapsed: 5s. ETA: 8s.Processed 112509 groups out of 253484. 44% done. Time elapsed: 6s. ETA: 7s.Processed 131818 groups out of 253484. 52% done. Time elapsed: 7s. ETA: 6s.Processed 150135 groups out of 253484. 59% done. Time elapsed: 8s. ETA: 5s.Processed 168615 groups out of 253484. 67% done. Time elapsed: 9s. ETA: 4s.Processed 186674 groups out of 253484. 74% done. Time elapsed: 10s. ETA: 3s.Processed 204775 groups out of 253484. 81% done. Time elapsed: 11s. ETA: 2s.Processed 222468 groups out of 253484. 88% done. Time elapsed: 12s. ETA: 1s.Processed 239571 groups out of 253484. 95% done. Time elapsed: 13s. ETA: 0s.Processed 253484 groups out of 253484. 100% done. Time elapsed: 13s. ETA: 0s.
```


``` r
head(lithologs)
#>    gicwellid longitude latitude  gr_elev bh_depth location_type location_source int_top_dep int_bot_dep  material material_desc colour
#>        <int>     <num>    <num>    <num>    <num>        <fctr>          <fctr>       <num>       <num>    <char>        <char> <char>
#> 1:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database      0.0000      0.3048   Topsoil          <NA>   <NA>
#> 2:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database      0.3048      2.4384      Till          <NA>  Brown
#> 3:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database      2.4384      4.2672      Clay          <NA>   Gray
#> 4:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database      4.2672      7.3152     Shale          <NA>   Gray
#> 5:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database      7.3152      8.2296 Sandstone Water Bearing   <NA>
#> 6:     40000 -112.3637 52.20076 831.7263   59.436          Well   Aenv database      8.2296     13.7160     Shale          <NA>   Gray
#>    waterbearing
#>          <lgcl>
#> 1:        FALSE
#> 2:        FALSE
#> 3:        FALSE
#> 4:        FALSE
#> 5:         TRUE
#> 6:        FALSE
```



``` r
future::plan("sequential")
```
