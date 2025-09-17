#' Flip Planimetric (1D) Cross Sections
#'
#' Flips planimetric cross sections so that the right side becomes
#' the left, and the right becomes the left.
#'
#' @param sxc Planimetric cross section (sxc) object.
#' @returns The original cross section where each section is flipped,
#' as if rotating each cross section by 180 degrees.
flip_plan <- function(plan) {
  xt_validate_plan(plan)
  # Extract and flip coordinates
  coords_list <- lapply(seq_along(plan), function(i) {
    n <- nrow(plan[[i]])
    plan[[i]][n:1, ]
  })
  sf::st_sfc(coords_list, crs = sf::st_crs(plan))
}
