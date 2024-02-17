
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
  filter = "gicwellid gt 40000 and gicwellid lt 41000"
)

reports <- awwid(
  "wellreports", 
  select = c("wellid", "wellreportid", "totaldepthdrilled"),
  filter = "wellreportid gt 40000 and wellreportid lt 41000"
)

lithologies <- awwid(
  name = "lithologies",
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

Several predefined queries that perform common processing tasks are also
included in the package:

``` r
lithologs <- query_lithologs(wells, reports, lithologies_df)
#> Warning: [extract] transforming vector data to the CRS of the raster
lithologs
#> # A tibble: 8,888 × 14
#>    gicwellid longitude latitude gr_elev bh_depth well_type location_type
#>        <int>     <dbl>    <dbl>   <dbl>    <dbl> <fct>     <fct>        
#>  1     40001     -113.     52.2    845.      149 Vertical  Well         
#>  2     40001     -113.     52.2    845.      149 Vertical  Well         
#>  3     40001     -113.     52.2    845.      149 Vertical  Well         
#>  4     40001     -113.     52.2    845.      149 Vertical  Well         
#>  5     40001     -113.     52.2    845.      149 Vertical  Well         
#>  6     40001     -113.     52.2    845.      149 Vertical  Well         
#>  7     40001     -113.     52.2    845.      149 Vertical  Well         
#>  8     40001     -113.     52.2    845.      149 Vertical  Well         
#>  9     40001     -113.     52.2    845.      149 Vertical  Well         
#> 10     40001     -113.     52.2    845.      149 Vertical  Well         
#> # ℹ 8,878 more rows
#> # ℹ 7 more variables: location_source <fct>, int_top_dep <dbl>,
#> #   int_bot_dep <dbl>, material <chr>, material_desc <chr>, colour <chr>,
#> #   waterbearing <lgl>
```
