library(terra)

x <- rast("~/Downloads/esrd_dem_100m.tif")
x <- aggregate(x, fact = 5)
x <- setNames(x, "dem")

dir.create("inst/extdata", recursive = TRUE)
writeRaster(x, "inst/extdata/dem.tif", overwrite = TRUE)
