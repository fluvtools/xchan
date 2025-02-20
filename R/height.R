#' @export
lb_height <- function(xs) {
  xs$left$bank[2] - xs$left$thalweg[2]
}

#' @export
rb_height <- function(xs) {
  xs$right$bank[2] - xs$right$thalweg[2]
}

# test_that("left and right bank widths add up to full width.", {
#   w <- xs_width(my_xs)
#   wl <- lb_width(my_xs)
#   wr <- rb_width(my_xs)
#   expect_equal(w, wl + wr)
# })
