#' Function to calculate a centerline from bankline polygon
#'
#' @param bankline Channel bankline polygon as a geometric object:
#' `sf`, `sfc`, or `SpatVector` polygons object.
#' @returns A centerline.
#' @details Wraps `centerline::cnt_path_guess()`.
#' @export
xt_generate_centerline <- function(bankline) {
  sf::st_geometry(centerline::cnt_path_guess(bankline, keep = 1))
}
