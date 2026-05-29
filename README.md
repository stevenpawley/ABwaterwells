
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ABwaterwells

<!-- badges: start -->

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

Create a connection object. This fetches the list of available tables
from the service.

``` r
library(ABwaterwells)

con <- AwwidQuery$new()
```

## Accessing tables by name

Each AWWID table has a dedicated method on the connection object. Use
your IDE’s tab completion to discover available tables — no need to
remember or type table names as strings.

``` r
# named methods accept filter, select, and top arguments
wells <- con$wells(
  select = c("gicwellid", "wellid", "longitude", "latitude"),
  filter = "gicwellid in (40000, 40001, 40002)"
)

reports <- con$wellreports(
  select = c("wellid", "wellreportid", "totaldepthdrilled"),
  filter = "wellreportid in (40000, 40001, 40002)"
)

# downloaded data is stored in the $data field
head(reports$data)
#> Key: <totaldepthdrilled>
#>    totaldepthdrilled wellid wellreportid
#>                <num>  <int>        <int>
#> 1:               149  40001        40001
#> 2:               195  40000        40000
#> 3:               268  40002        40002
```

Data in AWWID is stored in imperial units. Call `$metricate()` to
convert a table to metric:

``` r
wells_df   <- wells$metricate()
reports_df <- reports$metricate()

head(reports_df)
#>    totaldepthdrilled wellid wellreportid
#>              <units>  <int>        <int>
#> 1:       45.4152 [m]  40001        40001
#> 2:       59.4360 [m]  40000        40000
#> 3:       81.6864 [m]  40002        40002
```

## Looping over multiple tables

To loop over a set of table names programmatically, use `$request()`
directly. The `$tables` field lists every table available on the
service.

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
#> [25] "wellreports"                "analysisitem"              
#> [27] "borehole"                   "lithology"                 
#> [29] "otherseal"                  "perforation"               
#> [31] "pumptestitem"               "pumptest"                  
#> [33] "screen"                     "wellcasinglog"             
#> [35] "welldecommissioningdetail"  "wellmaterialslog"          
#> [37] "wellreport"
```

``` r
tables_to_download <- c("wells", "wellreports", "lithologies")

results <- lapply(tables_to_download, function(tbl) {
  con$request(tbl, top = 100)$metricate()
})
names(results) <- tables_to_download
```

## Caching

Repeated calls with the same arguments return the cached result
immediately. Use `$clear_cache()` to force a fresh download.

``` r
# second call returns instantly from cache
con$wells(
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

## Predefined queries

Several multi-table queries cover common workflows. Caching means shared
tables (e.g. `wells`) are only downloaded once across queries.

``` r
lithologs  <- con$query_lithologs()
waterlevel <- con$query_water_level()
```
