library(ABwaterwells)
library(dplyr)

table_names <- awwid_tables()

table_obj <- lapply(table_names, function(name) {
  x <- awwid(name, top = 1L)
  x <- tibble(attributes = names(x), type = sapply(x, class))
  attr(x, "awwid_tbl") <- tolower(name)
  return(x)
})

metadata <- setNames(table_obj, tolower(table_names))
usethis::use_data(metadata, internal = TRUE, overwrite = TRUE)
