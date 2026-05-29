# Fetch AWWID data from the AEPA OData server

Downloads a table from the AWWID OData service. The \`table\` argument
accepts either a character string or an \`awwid_table\` descriptor from
\[awwid_connect()\], which avoids typing table names as strings and
enables tab completion.

## Usage

``` r
awwid_tbl(table, ...)

# S3 method for class 'awwid_table'
awwid_tbl(
  table,
  filter = NULL,
  select = NULL,
  top = NULL,
  chunk_size = 10000L,
  .progress = FALSE
)

# S3 method for class 'character'
awwid_tbl(
  table,
  filter = NULL,
  select = NULL,
  top = NULL,
  chunk_size = 10000L,
  .progress = FALSE
)
```

## Arguments

- table:

  an \`awwid_table\` descriptor from \[awwid_connect()\], or a character
  string naming the table.

- filter:

  OData filter expression to select rows server-side.

- select:

  character vector of columns to return. By default all columns are
  returned.

- top:

  integer, return only the first n rows.

- chunk_size:

  integer, number of rows per download chunk when paginating. Reducing
  this value forces chunking on small tables, which is useful for
  testing.

- .progress:

  logical, show a progress bar when downloading chunks.

## Value

a tibble

## Details

Tables larger than \`chunk_size\` rows are downloaded in parallel chunks
using a deterministic \`\$orderby\` on the primary key to prevent gaps
or duplicates at chunk boundaries.
