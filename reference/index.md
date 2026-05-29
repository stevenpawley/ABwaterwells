# Package index

## Connection and data access

Connect to the AEPA AWWID OData service and download tables.
[`awwid_connect()`](https://stevenpawley.github.io/ABwaterwells/reference/awwid_connect.md)
returns a connection object whose named fields are table descriptors —
pass any field to
[`awwid_tbl()`](https://stevenpawley.github.io/ABwaterwells/reference/awwid_tbl.md)
for tab-completion without typing table names as strings.

- [`awwid_connect()`](https://stevenpawley.github.io/ABwaterwells/reference/awwid_connect.md)
  : Connect to the AEPA AWWID OData service
- [`awwid_tbl()`](https://stevenpawley.github.io/ABwaterwells/reference/awwid_tbl.md)
  : Fetch AWWID data from the AEPA OData server
- [`awwid_list_tables()`](https://stevenpawley.github.io/ABwaterwells/reference/awwid_list_tables.md)
  : List AWWID database tables

## Metric conversion

Data in AWWID is stored in imperial units.
[`metricate()`](https://stevenpawley.github.io/ABwaterwells/reference/metricate.md)
dispatches on the S3 class assigned by
[`awwid_tbl()`](https://stevenpawley.github.io/ABwaterwells/reference/awwid_tbl.md)
and applies the correct conversions for each table.

- [`metricate()`](https://stevenpawley.github.io/ABwaterwells/reference/metricate.md)
  : Convert a tibble of water well related data into metric units

## Predefined queries

Multi-table queries for common workflows. Each function accepts
already-downloaded, metricated tibbles as arguments so downloads and
processing are kept separate.

- [`query_lithologs()`](https://stevenpawley.github.io/ABwaterwells/reference/query_lithologs.md)
  : Predefined query to extract lithologs from the AWWID 'wells' and
  'lithologies' tables
- [`query_screens()`](https://stevenpawley.github.io/ABwaterwells/reference/query_screens.md)
  : Predefined query to extract the screen and perforation inteval
  ranges frp, the AWWID 'wells', 'wellreports', 'lithologies', 'screens'
  and 'perforations' tables
- [`query_staticwater()`](https://stevenpawley.github.io/ABwaterwells/reference/query_staticwater.md)
  : Predefined query to extract a table of static water levels

## Analysis

Functions for interpreting and classifying water well data.

- [`pick_bedrock()`](https://stevenpawley.github.io/ABwaterwells/reference/pick_bedrock.md)
  : Function to determine the depth to bedrock based on litholog
  intervals that are labelled as either 'Bedrock' or 'Surficial'
- [`allocate()`](https://stevenpawley.github.io/ABwaterwells/reference/allocate.md)
  : Allocate wells

## Datasets

Reference data bundled with the package.

- [`materials`](https://stevenpawley.github.io/ABwaterwells/reference/materials.md)
  : AWWID material codes to grain size estimates
