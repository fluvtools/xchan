target_crs <- 3005
fraser_bankline <- sf::read_sf(
  here::here("data-raw", "fraser_bankline.gpkg")
  )

fraser_bankline <- sf::st_transform(fraser_bankline, target_crs)
# Use a single Fraser River polygon that works cleanly with the demo workflow.
fraser_bankline <- sf::st_geometry(fraser_bankline[9, ])
usethis::use_data(fraser_bankline, overwrite = TRUE)
