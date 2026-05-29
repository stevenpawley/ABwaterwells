# Connect to the AEPA AWWID OData service

Fetches the list of available tables and returns a connection object
whose named fields are \`awwid_table\` descriptors — one per table. Pass
any field directly to \[awwid_tbl()\] instead of typing a table name as
a string.

## Usage

``` r
awwid_connect()
```

## Value

an \`awwid_connection\` object.

## Examples

``` r
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

awwid_tbl(con$wells, top = 10)
#> # A tibble: 10 × 29
#>    wellid drillingcompanyid gicwellid goawelltagnumber gownid longitude latitude
#>     <int>             <int>     <int> <chr>            <lgl>      <dbl>    <dbl>
#>  1  40000          10776530     40000 ""               NA         -112.     52.2
#>  2  40001          10776530     40001 ""               NA         -113.     52.2
#>  3  40002          10776530     40002 ""               NA         -113.     52.2
#>  4  40003             24120     40003 ""               NA         -114.     52.1
#>  5  40004             24120     40004 ""               NA         -114.     52.1
#>  6  40005             24349     40005 ""               NA         -113.     52.2
#>  7  40006             24671     40006 ""               NA         -114.     52.2
#>  8  40007             24428     40007 ""               NA         -114.     52.1
#>  9  40008             24428     40008 ""               NA         -114.     52.2
#> 10  40009             24349     40009 ""               NA         -114.     52.2
#> # ℹ 22 more variables: elevation <lgl>, gpsobtained <chr>,
#> #   elevationobtained <chr>, boundaryfrom <chr>, distancenorth <dbl>,
#> #   distancesouth <dbl>, distanceeast <dbl>, distancewest <dbl>, lsd <chr>,
#> #   section <chr>, township <chr>, range <chr>, meridian <chr>, lot <lgl>,
#> #   block <lgl>, plan <lgl>, additionaldescription <lgl>, validatedflag <int>,
#> #   submittedflag <int>, locationlockedflag <int>, createtimestamp <chr>,
#> #   updatetimestamp <chr>
awwid_tbl(con$wellreports, select = c("wellid", "totaldepthdrilled"))
#> # A tibble: 457,518 × 2
#>    totaldepthdrilled wellid
#>                <dbl>  <int>
#>  1                NA  78065
#>  2                NA 112842
#>  3                NA 115948
#>  4                NA 118027
#>  5                NA 119697
#>  6                NA 121581
#>  7                NA 123456
#>  8                NA 125308
#>  9                NA 139415
#> 10                NA 140790
#> # ℹ 457,508 more rows
```
