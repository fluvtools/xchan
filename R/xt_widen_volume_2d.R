xt_widen_volume_profile <- function(xs, volume, prop_left) {
  dv_left <- volume * prop_left
  dv_right <- volume - dw_left
  xs <- xt_widen_volume_profile_right(xs, dv_right)
  xt_widen_volume_profile_left(xs, dv_left)
}

xt_widen_volume_profile_right <- function(xs, volume) {
  checkmate::assert_numeric(volume, 0, len = 1, any.missing = FALSE)
  
  x_bank <- min(xs$banks[1])
  y_bank <- coords_interpolate(xs, x_bank)[2]
  nodes <- xs$coordinates
  
  x_new <- find_x_for_volume_right(
    v = volume,
    x0 = x_bank,
    topo = nodes,
    thalweg_height = y_bank,
    valley = "right"
  )
  
  xt_widen_width_profile_right(xs, x_new - x_bank)
}

xt_widen_volume_profile_left <- function(xs, volume) {
  xs <- flip_xs2d(xs)
  xs <- xt_widen_volume_profile_right(xs, volume)
  flip_xs2d(xs)
}
