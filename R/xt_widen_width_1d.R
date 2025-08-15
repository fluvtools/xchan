xt_widen_width_1d <- function(object, by, prop_left = 0.5) {
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  by_left <- by * prop_left
  by_right <- by - by_left
  object <- xt_widen_width_1d_right(object, by = by_right)
  object <- xt_widen_width_1d_right(xt_reverse_flow(object), by = by_left)
  xt_reverse_flow(object)
}

xt_widen_width_1d_right <- function(sxc, by) {
  checkmate::assert_class(sxc, "sxc")
  checkmate::assert_numeric(by)
  by <- vctrs::vec_recycle(by, length(sxc))
  one <- sf::st_length(sf::st_sfc(sxc)[1])
  one <- one / units::set_units(one, NULL)
  if (all(units::set_units(by, NULL) == 0)) {
    return(sxc)
  }
  for (i in seq_along(sxc)) {
    mat <- sxc[[i]]
    n <- nrow(mat)
    base_pt <- mat[1, 1:2]        # left
    end_pt <- mat[n, 1:2] # right
    vec <- end_pt - base_pt
    mag <- sqrt(sum(vec^2))
    unit_vec <- vec / mag
    translation_vec <- unit_vec * by[i]
    sxc[[i]][n, 1:2] <- units::set_units(
      sxc[[i]][n, 1:2] * one + translation_vec, NULL
    )
  }
  # vector <- sf::st_sfc(sxc)
  # mags <- sf::st_length(vector)
  # base_pts <- lapply(vector, \(v) {
  #   sf::st_point(sf::st_coordinates(v)[1, 1:2, drop = FALSE])
  # })
  # base_pts <- sf::st_sfc(base_pts)
  # unit_vec <- (vector - base_pts) / mags
  # translation_vec <- unit_vec * by
  # translation_pt <- lapply(translation_vec, \(v) {
  #   mat <- sf::st_coordinates(v)
  #   n <- nrow(mat)
  #   sf::st_point(mat[n, 1:2, drop = FALSE])
  # })
  # for (i in 1:length(sxc)) {
  #   n <- nrow(sxc[[i]])
  #   sxc[[i]][n, 1:2] <- sxc[[i]][n, 1:2] + translation_pt[[i]]
  # }
  sxc
}
