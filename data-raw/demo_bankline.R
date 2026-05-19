target_crs <- 3005
bankline <- sf::read_sf(
  here::here("data-raw", "Squamish_river.gpkg")
)

bankline <- sf::st_transform(bankline, target_crs)

bankline <- bankline |> sf::st_union()

usethis::use_data(bankline, overwrite = TRUE)
