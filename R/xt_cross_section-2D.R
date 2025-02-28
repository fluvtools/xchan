#' Make cross section
#' @examples
#' my_xs <- xt_cross_section(coords, 1, 12)
#' plot_(my_xs)
#'
#' xs_width(my_xs)
#' rb_height(my_xs)
#'
#' bigger_xs <- widen_right(my_xs, 2)
#' plot_(bigger_xs, add = TRUE)
#'
#' xs_width(bigger_xs)
#' rb_height(bigger_xs)
#'
#' plot_(widen_right(bigger_xs, 3), add = TRUE)
#' plot_(widen_left(bigger_xs, 3), add = TRUE)
#'
#' library(testthat)
#' test_that("left and right bank widths add up to full width.", {
#'   w <- xs_width(my_xs)
#'   wl <- lb_width(my_xs)
#'   wr <- rb_width(my_xs)
#'   expect_equal(w, wl + wr)
#' })
#' @export
xt_cross_section <- function(mat, x_lb, x_rb) {
  mat <- inject_bankpoint(mat, c(x_lb, x_rb))
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
      bank = get_points(coords_left, x_lb),
      thalweg = lb_thalwegs
    ),
    right = list(
      multiline = coords_right,
      bank = get_points(coords_right, x_rb),
      thalweg = rb_thalwegs
    )
  )
  new_sxc2d(l)
}

new_sxc2d <- function(l, ..., class = character()) {
  original_class <- class(l)
  structure(l, ..., class = c(class, "sxc2d", original_class))
}

#' Get thalwegs
#' @export
get_thalwegs <- function(mat) {
  mat[mat[, 2] == min(mat[, 2]), , drop = FALSE]
}

#' get point on cross section
#' @returns A matrix of points. Column one contains distances along the cross
#' section; column two, the elevations.
#' @export
get_points <- function(mat, x) {
  checkmate::assert_matrix(mat, min.cols = 2L, max.cols = 2L)
  rng <- range(mat[, 1])
  checkmate::assert_numeric(x, rng[1], rng[2])
  y <- approx(mat[, 1], mat[, 2], x)$y
  cbind(x, y)
}

#' Inject point
#'
#' Inject a point into a 2D cross section matrix,
#' potentially splitting a linesegment into two
#' if x doesn't already land on a node.
#'
#' I should have named this function differently, because it doesn't
#' inject a bankpoint into a 2D cross section. It just extends the
#' matrix of nodes.
#'
#' @param mat Matrix of nodes of distances along the cross section
#' (column one) and elevation (column two).
#' @param x Distance along cross section to add a new node to.
#' @returns The original matrix of nodes, with an additional
#' node correspoding to x in there.
#' @export
inject_bankpoint <- function(mat, x) {
  x_mat <- mat[, 1]
  x <- x[!(x %in% x_mat)]
  if (length(x) == 0) return(mat)
  new_points <- get_points(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat[order(new_mat[, 1]), ]
}

