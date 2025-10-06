xt_widen_volume_profile <- function(profile, volume, prop_left) {
  dv_left <- volume * prop_left
  dv_right <- volume - dv_left
  profile <- xt_widen_volume_profile_right(profile, dv_right)
  profile <- flip_profile(profile)
  profile <- xt_widen_volume_profile_right(profile, dv_left)
  flip_profile(profile)
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
