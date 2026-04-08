target_crs <- 3005

# Work in projected CRS so spacing/extent are in metres.
fraser_dem <- terra::rast(here::here("data-raw", "fraser_dem.tif"))

fraser_dem <- terra::project(fraser_dem, paste0("EPSG:", target_crs))
# SpatRaster objects do not serialize safely in package data; wrap before saving.
fraser_dem <- terra::wrap(fraser_dem, proxy = FALSE)
usethis::use_data(fraser_dem, overwrite = TRUE)
