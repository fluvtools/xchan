#' Inject Islands Into Cross Sections
#'
#' This function works with 1D cross sections (object of class "sxc") that have been
#' made by ignoring river islands, and injects the islands into the cross sections
#' so that the cross sections "know" about the islands.
#'
#' @param sxc List of 1-dimensional cross sections (an object of class "sxc"), each
#' only having a left and right bank (no islands bankpoints).
#' @param banklines Bankline polygon of a river that potentially contains islands.
#' @returns The original `sxc` object, with additional island bankpoints in between
#' the left and right bank points, where applicable. If there are no islands, the
#' cross section remains unchanged.
inject_islands <- function(sxc, banklines) {
  checkmate::assert_class(sxc, "sxc")
  checkmate::assert_class(banklines, "sfc")
  crs <- sf::st_crs(sxc)
  sxc <- sf::st_sfc(sxc)
  intersection_points <- sf::st_intersection(sxc, banklines)
  coords_list <- lapply(intersection_points, function(geom) {
    sf::st_coordinates(geom)[, 1:2, drop = FALSE]
  })
  multi_line <- sf::st_sfc(
    lapply(coords_list, function(x) sf::st_multilinestring(list(x))),
    crs = crs
  )
  xt_sxc(multi_line)
}
