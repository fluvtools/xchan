target_crs <- 3005
squamish_bankline <- sf::read_sf(
  here::here("data-raw", "Squamish_River.gpkg")
)

squamish_bankline <- sf::st_transform(squamish_bankline, target_crs)

squamish_bankline <- squamish_bankline |> sf::st_union()

usethis::use_data(squamish_bankline, overwrite = TRUE)
