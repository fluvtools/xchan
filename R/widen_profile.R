#' Erode a 2D cross section
#'
#' @param xs A single 2D cross section.
#' @param dw Change in width; single positive numeric.
#' @param prop_left Proportion of erosion occuring on the left bank
#' (the right bank will have `1 - prop_left` of the change in width).
#' @returns An eroded version of the input 2D cross section.
#' @details
#' Erosion Rules:
#'
#' 1. Sediment between the old bank and the new bank disappears. Even the
#'    old bank point disappears.
#' 2. Topography on the channel-side of the old bankpoint shifts over
#'    to meet the new bank point.
#'
#' Note that this will form a vertical "cliff" if eroding into an uphill
#' floodplain, or a "spike" if eroding into a downhill floodplain (where
#' the old channel bed appears above the new channel height). In all cases,
#' the height of the new bank ignores this bed topography, and is determined
#' by the outermost point.
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

#' @rdname xt_widen_2d
widen_profile_left <- function(profile, dw) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) {
    return(profile)
  }
  # Get left bank information
  left_bank_coords <- get_left_bank_coords(profile)
  x_old <- left_bank_coords[1]
  x_new <- x_old - dw
  x_extent <- min(profile$coordinates[, 1])
  if (x_new < x_extent) {
    stop(
      "Cannot widen profile: requested widening exceeds cross section extent."
    )
  }
  y_new <- coords_interpolate(profile$coordinates, x_new)[2]
  thalweg_coords <- get_min_thalweg_coords(profile)
  y_thal <- thalweg_coords[2]
  nodes <- profile$coordinates
  nodes <- inject_coords(nodes, x_new)

  # Update bank index to point to the new bank location
  new_bank_index <- which.min(abs(nodes[, 1] - x_new))
  profile$banks[1] <- new_bank_index

  if (y_new < y_thal) {
    warning(
      "River has eroded into a part of the floodplain that's lower in ",
      "elevation than the thalweg. The original thalweg is still being ",
      "interpreted as the thalweg."
    )
  }

  # Erosion rule 1: nodes in between old and new banks disappear,
  # including the old bank.
  x_in_between <- nodes[, 1] > x_new & nodes[, 1] <= x_old
  nodes <- nodes[!x_in_between, , drop = FALSE]
  # Erosion rule 2: nodes starting from the old bankpoint shift over
  # to the new bankpoint.
  thalweg_distances <- get_thalweg_distances(profile)
  x_river_part <- nodes[, 1] >= x_old & nodes[, 1] <= thalweg_distances[1]
  nodes[x_river_part, 1] <- nodes[x_river_part, 1] - dw
  profile$coordinates <- nodes

  # Update thalweg indices
  thalweg_indices <- profile$thalwegs
  thalweg_indices[1] <- which.min(abs(nodes[, 1] - (thalweg_distances[1] - dw)))
  profile$thalwegs <- thalweg_indices
  profile
}
