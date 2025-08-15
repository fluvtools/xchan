#' Make cross section
#' @examples
#' my_xs <- xt_cross_section(coords, 1, 12)
#' plot_(my_xs)
#'
#' xt_width(my_xs)
#' rb_height(my_xs)
#'
#' bigger_xs <- widen_right(my_xs, 2)
#' plot_(bigger_xs, add = TRUE)
#'
#' xt_width(bigger_xs)
#' rb_height(bigger_xs)
#'
#' plot_(widen_right(bigger_xs, 3), add = TRUE)
#' plot_(widen_left(bigger_xs, 3), add = TRUE)
#'
#' library(testthat)
#' test_that("left and right bank widths add up to full width.", {
#'   w <- xt_width(my_xs)
#'   wl <- lb_width(my_xs)
#'   wr <- rb_width(my_xs)
#'   expect_equal(w, wl + wr)
#' })
#' @export
xt_xs2d <- function(mat, x_lb, x_rb) {
  mat <- inject_2d_points(mat, c(x_lb, x_rb))
  # Subset the matrix to only include points between x_lb and x_rb
  mat_subset <- mat[mat[, 1] >= x_lb & mat[, 1] <= x_rb, , drop = FALSE]
  thalwegs <- get_thalwegs(mat_subset)
  x_thalwegs <- thalwegs[, 1]
  coords_left <- mat[mat[, 1] <= min(x_thalwegs), , drop = FALSE]
  coords_right <- mat[mat[, 1] >= max(x_thalwegs), , drop = FALSE]
  coords_left_bed <- coords_left[coords_left[, 1] >= x_lb, , drop = FALSE]
  coords_right_bed <- coords_right[coords_right[, 1] <= x_rb, , drop = FALSE]
  lb_thalwegs <- get_thalwegs(coords_left_bed)
  rb_thalwegs <- get_thalwegs(coords_right_bed)
  l <- list(
    left = list(
      multiline = coords_left,
      bank = get_2d_points(coords_left, x_lb),
      thalweg = lb_thalwegs
    ),
    right = list(
      multiline = coords_right,
      bank = get_2d_points(coords_right, x_rb),
      thalweg = rb_thalwegs
    )
  )
  new_xs2d(l)
}

new_xs2d <- function(l, ..., class = character()) {
  structure(l, ..., class = c(class, "xs2d"))
}
