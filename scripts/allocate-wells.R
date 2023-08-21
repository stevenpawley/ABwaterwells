library(dplyr)
library(ABwaterwells)
library(pins)
library(AzureStor)
library(tidymodels)
library(bedrocktopo)

future::plan("multisession")

# request awwid tables from AEP web server ----
# wells tables
wells = request_awwid("wells") |>
  metricate()

board_local() |>
  pin_write(wells, "awwid-wells")

reports = request_awwid("wellreports") |>
  metricate()

board_local() |>
  pin_write(reports, "awwid-wellreports")

# screens and perforations
screens = request_awwid("screens") |>
  metricate()

board_local() |>
  pin_write(metricate, "awwid-screens")

perforations = request_awwid("perforations") |>
  metricate()

board_local() |>
  pin_write(perforations, "awwid-perforations")

# lithologies
lithologies = request_awwid("lithologies") |>
  metricate()

board_local() |>
  pin_write(lithologies, "awwid-lithologies")

# data preparation ----
# create a standalone lithologs table
lithologs = query_awwid_lithologs(wells, reports, lithologies)

# aggregate the complete range of depths for the screened/perforated interval
# of each well (uses drilled depth if missing)
screened = query_awwid_screens(wells, reports, screens, perforations)

# perform allocation (bedrock/surficial) ----
# download the trained NLP model
endp = blob_endpoint(
  endpoint = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
  key = Sys.getenv("AZURE_KEY")
)
bl = blob_container(endp, "ds-spatial-processed")
board = board_azure(bl, "quaternary-picks")

model = board |>
  pin_read("model-nlp")

# perform allocation
allocated = allocate(lithologs, screens, model)
