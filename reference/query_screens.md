# Predefined query to extract the screen and perforation inteval ranges frp, the AWWID 'wells', 'wellreports', 'lithologies', 'screens' and 'perforations' tables

Predefined query to extract the screen and perforation inteval ranges
frp, the AWWID 'wells', 'wellreports', 'lithologies', 'screens' and
'perforations' tables

## Usage

``` r
query_screens(
  wells,
  wells_reports,
  screens,
  perforations,
  .aggregate = TRUE,
  .assumed_top = 5
)
```

## Arguments

- wells:

  tibble of the 'wells' table from AWWID

- wells_reports:

  tibble of the 'wellreports' table from AWWID

- screens:

  tibble of the 'screens' table from AWWID

- perforations:

  tibble of the 'perforations' table from AWWID

- .aggregate:

  logical. If TRUE (default) then the full depth range of
  screens/perforations for each well is returned. If FALSE, then all the
  individual screen/perforation intervals are returned.

- .assumed_top:

  numeric. If a well has no screens or perforations, then a 5 m screen
  interval is created at the total depth drilled. This parameter sets
  the length of that interval in metres.

## Value

tibble of processed AWWID litholog data
