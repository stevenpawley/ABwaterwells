library(tidyverse)

materials <- read_csv("data-raw/materials.csv")
usethis::use_data(materials, overwrite = TRUE)
