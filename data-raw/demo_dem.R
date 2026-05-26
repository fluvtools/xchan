# The source GeoTIFF is intentionally not tracked in git because it is large.
# Recreate it with data-raw/download_dem.py before running this script.

target_crs <- 3005

# Work in projected CRS so spacing/extent are in metres.
Squamish_dem <- terra::rast(here::here("data-raw", "Squamish_DEM-1m.tif"))
Squamish_dem <- terra::project(Squamish_dem, paste0("EPSG:", target_crs))

# SpatRaster objects do not serialize safely in package data; wrap before saving.
Squamish_dem <- terra::wrap(Squamish_dem, proxy = FALSE)

usethis::use_data(Squamish_dem, overwrite = TRUE)
