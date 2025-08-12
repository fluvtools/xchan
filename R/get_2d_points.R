#' get point on cross section
#' @returns A matrix of points. Column one contains distances along the cross
#' section; column two, the elevations.
#' @export
get_2d_points <- function(mat, x) {
  checkmate::assert_matrix(mat, min.cols = 2L, max.cols = 2L)
  rng <- range(mat[, 1])
  checkmate::assert_numeric(x, rng[1], rng[2])
  y <- approx(mat[, 1], mat[, 2], x)$y
  cbind(x, y)
}
