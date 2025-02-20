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
  coords_left <- mat[mat[, 1] <= min(x_thalwegs), ]
  coords_right <- mat[mat[, 1] >= max(x_thalwegs), ]
  lb_thalwegs <- get_thalwegs(coords_left)
  rb_thalwegs <- get_thalwegs(coords_right)
  l <- list(
    left = list(
      multiline = coords_left,
      bank = get_points(coords_left, x_lb),
      thalweg = get_thalwegs(coords_left)
    ),
    right = list(
      multiline = coords_right,
      bank = get_points(coords_right, x_rb),
      thalweg = get_thalwegs(coords_right)
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
#' @export
get_points <- function(mat, x) {
  y <- approx(mat[, 1], mat[, 2], x)$y
  cbind(x, y)
}

#' Inject point
#'
#' Inject bankpoint, potentially splitting a linesegment into two
#' if x doesn't already land on a node.
#' @export
inject_bankpoint <- function(mat, x) {
  x <- x[!(x %in% mat[, 1])]
  if (length(x) == 0) return(mat)
  new_points <- get_points(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat[order(new_mat[, 1]), ]
}

