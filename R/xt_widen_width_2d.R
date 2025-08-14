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
  x_old <- xs$left$bank[1]
  x_new <- x_old - dw
  left_nodes <- xs$left$multiline
  left_nodes <- inject_2d_points(left_nodes, x_new)
  xs$left$bank <- get_2d_points(left_nodes, x_new)
  if (xs$left$bank[2] < xs$left$thalweg[2]) {
    warning(
      "River has eroded into a part of the floodplain that's lower in ",
      "elevation than the thalweg. The original thalweg is still being ",
      "interpreted as the thalweg."
    )
  }
  xs$left$thalweg[1] <- xs$left$thalweg[1] - dw
  # Erosion rule 1: nodes in between old and new banks disappear,
  # including the old bank.
  x_in_between <- left_nodes[, 1] > x_new & left_nodes[, 1] <= x_old
  left_nodes <- left_nodes[!x_in_between, , drop = FALSE]
  # Erosion rule 2: nodes starting from the old bankpoint shift over
  # to the new bankpoint.
  x_river_part <- left_nodes[, 1] >= x_old
  left_nodes[x_river_part, 1] <- left_nodes[x_river_part, 1] - dw
  xs$left$multiline <- left_nodes
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
