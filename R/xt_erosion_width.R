#' @export
xt_erosion_width <- function(xs, dv, prop_left = 0.5) {
  dv_left <- dv * prop_left
  dv_right <- dv - dw_left
  dw1 <- xt_erosion_width_right(xs, dv_right)
  dw2 <- xt_erosion_width_right(flip_xs2d(xs), dv_left)
  dw1 + dw2
}

xt_erosion_width_right <- function(xs, dv) {
  checkmate::assert_numeric(dv, 0, len = 1, any.missing = FALSE)
  x_bank <- xs$right$bank[1]
  y_bank <- xs$right$bank[2]
  right_nodes <- xs$right$multiline
  x_new <- find_x_for_volume_right(
    v = dv,
    x0 = x_bank,
    topo = right_nodes,
    thalweg_height = y_bank,
    valley = "right"
  )
  x_new - x_bank
}
