target_crs <- 3005

# Work in projected CRS so spacing/extent are in metres.
dem <- terra::rast(here::here("data-raw", "hrdem_1m_poi.tif"))

dem <- terra::project(dem, paste0("EPSG:", target_crs))
# SpatRaster objects do not serialize safely in package data; wrap before saving.
dem <- terra::wrap(dem, proxy = FALSE)

usethis::use_data(dem, overwrite = TRUE)
