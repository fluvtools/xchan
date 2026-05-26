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

#' Interpolate elevation at `x` using only points at or to the left of `x`
#' @noRd
coords_interpolate_left <- function(coords, x) {
  checkmate::assert_matrix(coords, mode = "numeric", ncols = 2)
  checkmate::assert_numeric(x)
  left <- coords[coords[, 1] <= x, , drop = FALSE]
  if (!nrow(left)) {
    return(coords_interpolate(coords, x))
  }
  if (nrow(left) == 1L) {
    return(cbind(x, left[1, 2]))
  }
  y <- stats::approx(left[, 1], left[, 2], xout = x, rule = 2)$y
  cbind(x, y)
}
