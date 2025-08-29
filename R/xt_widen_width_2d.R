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
xt_widen_width_2d <- function(xs, dw, prop_left) {
  checkmate::assert_numeric(dw, 0)
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  dw_left <- prop_left * dw
  dw_right <- dw - dw_left
  xs <- xt_widen_width_2d_right(xs, dw_right)
  xt_widen_width_2d_left(xs, dw_left)
}

#' @rdname xt_widen_2d
xt_widen_width_2d_left <- function(xs, dw) {
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) return(xs)
  xs$banks <- sort(xs$banks)
  xs$thalwegs <- sort(xs$thalwegs)
  nodes <- xs$coordinates
  # Get left bank information
  x_old <- xs$banks[1]
  x_new <- x_old - dw
  y_new <- get_2d_points(nodes, x_new)[2]
  y_thal <- get_2d_points(nodes, xs$thalwegs[1])[2]
  nodes <- inject_2d_points(nodes, x_new)
  xs$banks[1] <- x_new
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
  x_river_part <- nodes[, 1] >= x_old & nodes[, 1] <= xs$thalwegs[1]
  nodes[x_river_part, 1] <- nodes[x_river_part, 1] - dw
  xs$coordinates <- nodes
  xs$thalwegs[1] <- xs$thalwegs[1] - dw
  xs
}

#' @rdname xt_widen_2d
xt_widen_width_2d_right <- function(xs, dw) {
  checkmate::assert_numeric(dw, 0, len = 1)
  if (dw == 0) return(xs)
  xs <- flip_xs2d(xs)
  xs <- xt_widen_left_2d(xs, dw)
  flip_xs2d(xs)
}
