# Predefined query to extract lithologs from the AWWID 'wells' and 'lithologies' tables

Predefined query to extract lithologs from the AWWID 'wells' and
'lithologies' tables

## Usage

``` r
query_lithologs(wells, well_reports, lithologies)
```

## Arguments

- wells:

  tibble of the 'wells' table from AWWID. The 'gicwellid', 'wellid',
  'longitude', and 'latitude' columns have to be present.

- well_reports:

  tibble of the 'wellreports' table from AWWID. Only the columns
  'wellreportid', 'wellid', and 'totaldepthdrilled' are required.

- lithologies:

  tibble of the 'lithologies' table from AWWID. The 'wellreportid',
  'material', 'description', 'lithdepthfrom', 'lithdepthto', 'colour',
  'waterbearing' columns are required.

## Value

tibble of processed AWWID litholog data
