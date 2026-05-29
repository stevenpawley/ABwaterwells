test_that("awwid_connect returns an awwid_connection with awwid_table fields", {
  con <- awwid_connect()
  expect_s3_class(con, "awwid_connection")
  expect_type(con$wells, "character")
  expect_true(length(con) > 0)
})

test_that("awwid_tbl via awwid_table descriptor returns the correct S3 class after metricate", {
  con <- awwid_connect()

  tables <- names(con)
  for (tbl in tables) {
    df <- awwid_tbl(tbl, top = 10) |> metricate()
    expect_true(inherits(df, tbl))
    expect_gt(nrow(df), 0, label = tbl)
  }
})

test_that("awwid_tbl via character string also works", {
  df <- awwid_tbl("wells", top = 10)
  expect_true(inherits(df, "wells"))
  expect_gt(nrow(df), 0)
})

test_that("awwid_tbl rejects unknown table names", {
  expect_error(awwid_tbl("NotATable"), regexp = "`table` must be one of")
})

test_that("awwid_tbl row count matches @odata.count", {
  con <- awwid_connect()
  name <- "materialoptions"
  url <- "https://data.environment.alberta.ca/Services/EDW/waterwellsdatamart/odata"

  count_url <- paste0(url, "/", name, "?$count=true&$top=0")
  resp_body <- httr2::request(count_url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expected_count <- as.integer(resp_body[["@odata.count"]])

  result <- awwid_tbl(con$materialoptions)
  expect_equal(nrow(result), expected_count)
})

test_that("chunked download matches single-request download and has no duplicates", {
  con <- awwid_connect()

  direct <- awwid_tbl(con$materialoptions)
  chunked <- awwid_tbl(con$materialoptions, chunk_size = 2L)

  key_col <- names(direct)[1]
  direct <- direct[order(direct[[key_col]]), ]
  chunked <- chunked[order(chunked[[key_col]]), ]

  expect_equal(nrow(chunked), nrow(direct))
  expect_false(anyDuplicated(chunked[[key_col]]) > 0)
  expect_equal(chunked[[key_col]], direct[[key_col]])
})
