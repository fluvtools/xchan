#' Obtain interpolated points along a cross section
#' @returns A matrix of points. Column one contains distances along the cross
#' section; column two, the elevations.
coords_interpolate <- function(profile, x) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_numeric(x)
  mat <- coords_all(profile)
  rng <- range(mat[, 1])
  # Need ties = "ordered" so that, in the case of a vertical cliff,
  # interpolation to the left uses the first of the ties, and right uses
  # the last of the ties.
  y <- stats::approx(mat[, 1], mat[, 2], x, ties = "ordered")$y
  cbind(x, y)
}
