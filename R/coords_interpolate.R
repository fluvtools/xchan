#' Obtain interpolated points along a cross section
#' @returns A matrix of points. Column one contains distances along the cross
#' section; column two, the elevations.
#' @noRd
coords_interpolate <- function(coords, x) {
  checkmate::assert_matrix(coords, mode = "numeric", ncols = 2)
  checkmate::assert_numeric(x)
  # Need ties = "ordered" so that, in the case of a vertical cliff,
  # interpolation to the left uses the first of the ties, and right uses
  # the last of the ties.
  y <- stats::approx(coords[, 1], coords[, 2], x, ties = "ordered", rule = 2)$y
  cbind(x, y)
}
