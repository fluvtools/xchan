target_crs <- 3005
Squamish_bankline <- sf::read_sf(
  here::here("data-raw", "Squamish_River.gpkg")
)

Squamish_bankline <- sf::st_transform(Squamish_bankline, target_crs)

Squamish_bankline <- Squamish_bankline |> sf::st_union()

usethis::use_data(Squamish_bankline, overwrite = TRUE)
