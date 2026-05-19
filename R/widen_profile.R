#' Erode a 2D cross section
#'
#' @param profile A single 2D cross section (`xs_profile`).
#' @param dw Change in width; single positive numeric.
#' @param prop_left Proportion of erosion occuring on the left bank
#' (the right bank will have `1 - prop_left` of the change in width).
#' @returns An eroded version of the input 2D cross section.
#' @details
#' Profile erosion follows three rules (applied on each bank via [flip_profile()]
#' for the opposite side):
#'
#' 1. Ground between the old and new bank positions is removed (the old bank
#'    point is removed as well).
#' 2. The **left-side channel** (topography between the left bank and the
#'    leftmost thalweg) slides left by `dw`, preserving its shape. The opposite
#'    bank is fixed.
#' 3. The span between the leftmost and rightmost thalwegs widens by `dw` on the
#'    eroded side; the new strip is filled with a flat channel bottom at the
#'    thalweg elevation.
#'
#' A vertical bank face is placed at the new bank: its elevation is taken from
#' the pre-erosion ground surface at that distance (linear interpolation along
#' the profile). Material below the thalweg elevation in the eroded strip does
#' not count toward [xt_erosion_volume()].
#'
#' Eroding into a floodplain depression below the thalweg yields a warning and
#' a cliff down to the channel; eroding into higher ground yields a cliff that
#' rises above the channel.
#' @rdname xt_widen_2d
widen_profile <- function(
  profile,
  dw,
  prop_left
) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_number(dw, lower = 0)
  checkmate::assert_number(prop_left, lower = 0, upper = 1)
  dw_left <- prop_left * dw
  dw_right <- dw - dw_left
  profile <- widen_profile_left(profile, dw_left)
  profile <- flip_profile(profile)
  profile <- widen_profile_left(profile, dw_right)
  flip_profile(profile)
}

#' @noRd
set_node_at_x <- function(nodes, x, y = NULL) {
  nodes <- inject_coords(nodes, x)
  idx <- which.min(abs(nodes[, 1] - x))
  if (!is.null(y)) {
    nodes[idx, 2] <- y
  }
  list(nodes = nodes, idx = idx)
}

#' @noRd
profile_indices_at_distances <- function(nodes, distances) {
  vapply(
    distances,
    function(xd) which.min(abs(nodes[, 1] - xd)),
    integer(1L)
  )
}

#' @rdname xt_widen_2d
widen_profile_left <- function(profile, dw) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) {
    return(profile)
  }

  nodes_orig <- profile$coordinates
  left_bank <- get_left_bank_coords(profile)
  right_bank <- get_right_bank_coords(profile)
  x_old <- left_bank[1]
  x_new <- x_old - dw
  x_extent <- min(nodes_orig[, 1])
  if (x_new < x_extent) {
    stop(
      "Cannot widen profile: requested widening exceeds cross section extent.",
      call. = FALSE
    )
  }

  thal_d_old <- get_thalweg_distances(profile)
  x_left_thal <- min(thal_d_old)
  y_bed <- profile$thalweg_elev
  y_cliff <- coords_interpolate(nodes_orig, x_new)[2]

  if (y_cliff < y_bed) {
    warning(
      "River has eroded into a part of the floodplain that's lower in ",
      "elevation than the thalweg. The original thalweg is still being ",
      "interpreted as the thalweg.",
      call. = FALSE
    )
  }

  thal_d_new <- thal_d_old
  on_left_channel <- thal_d_old >= x_left_thal & thal_d_old <= x_old
  thal_d_new[on_left_channel] <- thal_d_old[on_left_channel] - dw

  nodes <- nodes_orig
  x_remove <- nodes[, 1] > x_new & nodes[, 1] <= x_old
  nodes <- nodes[!x_remove, , drop = FALSE]

  left_channel <- nodes[, 1] >= x_left_thal & nodes[, 1] <= x_old
  nodes[left_channel, 1] <- nodes[left_channel, 1] - dw

  nodes <- set_node_at_x(nodes, x_left_thal, y_bed)$nodes
  nodes <- set_node_at_x(nodes, x_new, y_cliff)$nodes
  nodes <- set_node_at_x(nodes, right_bank[1], right_bank[2])$nodes
  nodes <- nodes[order(nodes[, 1]), , drop = FALSE]

  lb_idx <- profile_indices_at_distances(nodes, x_new)
  rb_idx <- profile_indices_at_distances(nodes, right_bank[1])
  nodes[lb_idx, 2] <- y_cliff
  nodes[rb_idx, ] <- right_bank
  flat_idx <- profile_indices_at_distances(nodes, x_left_thal)
  nodes[flat_idx, 2] <- y_bed

  profile$coordinates <- nodes
  profile$banks <- c(lb_idx, rb_idx)
  profile$thalwegs <- profile_indices_at_distances(nodes, thal_d_new)
  profile$thalweg_elev <- min(nodes[profile$thalwegs, 2])
  profile
}
