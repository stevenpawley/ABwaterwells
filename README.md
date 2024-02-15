
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
library(glue)
plan("multisession")
```

To view the available tables:

``` r
awwid_tables()
#>  [1] "AnalysisItems"              "Boreholes"                 
#>  [3] "MaterialOptions"            "WellCasingLogs"            
#>  [5] "PlacementMethodOptions"     "ChemicalAnalysis"          
#>  [7] "Drillers"                   "DrillingCompanies"         
#>  [9] "Elements"                   "GeophysicalLogs"           
#> [11] "Lithologies"                "OtherSeals"                
#> [13] "Perforations"               "PumpTests"                 
#> [15] "PumpTestItems"              "Screens"                   
#> [17] "Wells"                      "UnitOptions"               
#> [19] "PlugMaterialOptions"        "CasingStatus"              
#> [21] "WellMaterialsLogs"          "WellDecommissioningDetails"
#> [23] "WellDecommissioningReasons" "WellOwners"                
#> [25] "WellReports"
```

``` r
# request individual tables
wells <- awwid(
  name = "wells", 
  select = c("gicwellid", "wellid", "longitude", "latitude"),
  filter = "gicwellid in (40000, 40001, 40002, 40003)"
)

reports <- awwid(
  "wellreports", 
  select = c("wellid", "wellreportid", "totaldepthdrilled"),
  filter = "wellreportid in (40000, 40001, 40002, 40003)"
)

lithologies <- awwid(
  name = "lithologies",
  filter = "wellreportid eq 40000 or wellreportid eq 40001"
)

head(wells)
#> # A tibble: 4 × 4
#>   gicwellid latitude longitude wellid
#>       <int>    <dbl>     <dbl>  <int>
#> 1     40000     52.2     -112.  40000
#> 2     40001     52.2     -113.  40001
#> 3     40002     52.2     -113.  40002
#> 4     40003     52.1     -114.  40003
```

Data in the [Alberta Water Well Information
Database](https://www.alberta.ca/alberta-water-well-information-database)
is stored in imperial units. The `metricate` method can be used to
automatically convert each table into metric units:

``` r
# the 'metricate' method automatically converts fields from each table into metric units
lithologies_df <- lithologies |> 
  metricate()

head(lithologies_df)
#> # A tibble: 6 × 10
#>   lithologyid wellreportid lithdepthfrom lithdepthto material  description  
#>         <int>        <int>           [m]         [m] <chr>     <chr>        
#> 1    13050069        40000         0           0.305 Topsoil   <NA>         
#> 2    12743972        40000         0.305       2.44  Till      <NA>         
#> 3    12856371        40000         2.44        4.27  Clay      <NA>         
#> 4    12743973        40000         4.27        7.32  Shale     <NA>         
#> 5    12743974        40000         7.32        8.23  Sandstone Water Bearing
#> 6    12743975        40000         8.23       13.7   Shale     <NA>         
#> # ℹ 4 more variables: waterbearing <lgl>, colour <chr>, createtimestamp <dttm>,
#> #   updatetimestamp <dttm>
```

Several predefined queries that perform common processing tasks are also
included in the package:

``` r
lithologs <- query_lithologs(wells, reports, lithologies_df)
#> Warning: [extract] transforming vector data to the CRS of the raster
lithologs
#> # A tibble: 35 × 14
#>    gicwellid longitude latitude gr_elev bh_depth well_type location_type
#>        <int>     <dbl>    <dbl>   <dbl>    <dbl> <fct>     <fct>        
#>  1     40000     -112.     52.2    832.      195 Vertical  Well         
#>  2     40000     -112.     52.2    832.      195 Vertical  Well         
#>  3     40000     -112.     52.2    832.      195 Vertical  Well         
#>  4     40000     -112.     52.2    832.      195 Vertical  Well         
#>  5     40000     -112.     52.2    832.      195 Vertical  Well         
#>  6     40000     -112.     52.2    832.      195 Vertical  Well         
#>  7     40000     -112.     52.2    832.      195 Vertical  Well         
#>  8     40000     -112.     52.2    832.      195 Vertical  Well         
#>  9     40000     -112.     52.2    832.      195 Vertical  Well         
#> 10     40000     -112.     52.2    832.      195 Vertical  Well         
#> # ℹ 25 more rows
#> # ℹ 7 more variables: location_source <fct>, int_top_dep <dbl>,
#> #   int_bot_dep <dbl>, material <chr>, material_desc <chr>, colour <chr>,
#> #   waterbearing <lgl>
```
