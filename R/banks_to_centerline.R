banks_to_centerline <- function(banks) {
  sf::st_geometry(centerline::cnt_path_guess(banks, keep = 1))
}
