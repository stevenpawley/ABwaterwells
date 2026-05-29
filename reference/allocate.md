# Allocate wells

Allocate wells

## Usage

``` r
allocate(lithologs, screens, model)
```

## Arguments

- lithologs:

  a lithologs table derived from the \`query_awwid_lithologs\` function

- screens:

  a screens tabke derived from the \`query_awwid_screens\` function

- model:

  a \`vetiver\` trained machine learning model that returns a data.frame
  like object with the predicted response as a '.pred_class' column

## Value

tibble of allocated screens
