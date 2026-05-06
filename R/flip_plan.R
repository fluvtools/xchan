#' Flip Planimetric (1D) Cross Sections
#'
#' Flips planimetric cross sections end-for-end so the former right bank
#' vertex comes first and the former left bank vertex comes last.
#'
#' @param sxc Planimetric cross section (sxc) object.
#' @returns The original cross section where each section is flipped,
#' as if rotating each cross section by 180 degrees.
#' @noRd
flip_plan <- function(plan) {
  xt_validate_plan(plan)
  coords_list <- lapply(seq_along(plan), function(i) {
    m <- sf::st_coordinates(plan[i, , drop = FALSE])
    m <- m[, 1:2, drop = FALSE]
    n <- nrow(m)
    if (n < 2L) {
      stop("Each plan line must have at least two coordinates.", call. = FALSE)
    }
    sf::st_linestring(m[n:1, , drop = FALSE])
  })
  sf::st_sfc(coords_list, crs = sf::st_crs(plan))
}
