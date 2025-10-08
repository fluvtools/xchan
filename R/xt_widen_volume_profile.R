xt_widen_volume_profile <- function(profile, dv, prop_left) {
  dv_left <- dv * prop_left
  dv_right <- dv - dv_left
  profile <- xt_widen_volume_profile_right(profile, dv_right)
  profile <- flip_profile(profile)
  profile <- xt_widen_volume_profile_right(profile, dv_left)
  flip_profile(profile)
}

xt_widen_volume_profile_right <- function(profile, dv) {
  checkmate::assert_numeric(dv, 0, len = 1, any.missing = FALSE)

  left_bank_coords <- get_left_bank_coords(profile)
  x_bank <- left_bank_coords[1]
  y_bank <- left_bank_coords[2]
  nodes <- profile$coordinates

  x_new <- find_x_for_volume_right(
    v = dv,
    x0 = x_bank,
    topo = nodes,
    thalweg_height = y_bank,
    valley = "right"
  )

  xt_widen_width_profile_right(profile, x_new - x_bank)
}
