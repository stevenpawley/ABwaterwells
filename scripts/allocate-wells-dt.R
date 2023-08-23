library(data.table)
library(ABwaterwells)
library(pins)
library(AzureStor)
library(terra)
library(bedrocktopo)

future::plan("multisession")

# retrieve water well data ----
con = dbAwwid$new()
lithologs = con$query_lithologs()
screens = con$query_screens()

# pin locally
pin_write(board_local(), lithologs, "awwid-lithologs-dt")
pin_write(board_local(), screens, "awwid-screens-dt")

# perform allocation (bedrock/surficial) ----
# download the trained NLP model
endp = blob_endpoint(
  endpoint = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
  key = Sys.getenv("AZURE_KEY")
)

bl = blob_container(endp, "ds-spatial-processed")
board = board_azure(bl, "quaternary-picks")
model = pin_read(board, "model-nlp-mlr3")

# prep
lithologs = na.omit(lithologs, cols = c("latitude", "longitude"))
lithologs_sf = vect(lithologs, geom = c("longitude", "latitude"), crs = "epsg:4326")

lithologs_sf$x = crds(lithologs_sf)[, 1]
lithologs_sf$y = crds(lithologs_sf)[, 2]
lithologs_tm = as.data.table(lithologs_sf)

# model prediction
predicted_intervals = model$predict_newdata(lithologs_tm)
predicted_tops = pick_bedrock(predicted_intervals, lithologs_tm)

predicted_logs = cbind(lithologs_tm, as.data.table(predicted_intervals))
predicted_logs = predicted_tops[as.data.table(predicted_intervals), on = "gicwellid"]

predicted_picks = predicted_logs[, .SD[1], by = gicwellid]
predicted_picks = predicted_picks[, .("gicwellid", "bedrock_dep")]
predicted_picks[, litholog_present := TRUE]

