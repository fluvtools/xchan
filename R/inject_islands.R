#' Inject Islands Into Cross Sections
#'
#' This function works with planimetric cross sections
#' (sfc_LINESTRING object) made by ignoring river islands, and injects the
#' islands into the cross sections so that the cross sections "know" about the
#' islands.
#'
#' @param plan Planimetric cross sections (sfc_LINESTRING object).
#' @param banklines Bankline polygon of a river that potentially
#'   contains islands.
#' @returns The original `plan` object, with additional island bankpoints
#'   in between the left and right bank points, where applicable. If there
#'   are no islands, the cross section remains unchanged.
#' @noRd
inject_islands <- function(plan, banklines) {
  checkmate::assert_class(plan, "sfc_LINESTRING")
  checkmate::assert_class(banklines, "sfc")
  crs <- sf::st_crs(plan)
  plan <- sf::st_sfc(plan)
  intersection_points <- sf::st_intersection(plan, banklines)
  coords_list <- lapply(intersection_points, function(geom) {
    sf::st_coordinates(geom)[, 1:2, drop = FALSE]
  })
  sf::st_sfc(
    lapply(coords_list, function(x) sf::st_multilinestring(list(x))),
    crs = crs
  )
}
