# The source GeoTIFF is intentionally not tracked in git because it is large.
# Recreate it with data-raw/download_dem.py before running this script.

target_crs <- 3005

# Work in projected CRS so spacing/extent are in metres.
squamish_dem <- terra::rast(here::here("data-raw", "Squamish_DEM-1m.tif"))
squamish_dem <- terra::project(squamish_dem, paste0("EPSG:", target_crs))

# Reduce DEM resolution for file size compatibility,
squamish_dem <- terra::aggregate(squamish_dem, fact = 4, fun = "mean")

# SpatRaster objects do not serialize safely in package data; wrap before saving.
squamish_dem <- terra::wrap(squamish_dem, proxy = FALSE)

usethis::use_data(squamish_dem, overwrite = TRUE)
