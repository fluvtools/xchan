xt_widen_volume_2d <- function(xs, volume, prop_left) {
  dv_left <- volume * prop_left
  dv_right <- volume - dw_left
  xs <- xt_widen_volume_2d_right(xs, dv_right)
  xt_widen_volume_2d_left(xs, dv_left)
}

xt_widen_volume_2d_right <- function(xs, volume) {
  checkmate::assert_numeric(volume, 0, len = 1, any.missing = FALSE)
  x_bank <- xs$right$bank[1]
  y_bank <- xs$right$bank[2]
  right_nodes <- xs$right$multiline
  x_new <- find_x_for_volume_right(
    v = volume,
    x0 = x_bank,
    topo = right_nodes,
    thalweg_height = y_bank,
    valley = "right"
  )
  xt_widen_width_2d_right(xs, x_new - x_bank)
}

xt_widen_volume_2d_left <- function(xs, volume) {
  xs <- flip_xs2d(xs)
  xs <- xt_widen_volume_2d_right(xs, volume)
  flip_xs2d(xs)
}
