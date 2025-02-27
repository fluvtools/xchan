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
#' @export
xt_widen_left_2d <- function(xs, dw) {
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) return(xs)
  x_old <- xs$left$bank[1]
  x_new <- x_old - dw
  left_nodes <- xs$left$multiline
  left_nodes <- inject_bankpoint(left_nodes, x_new)
  xs$left$bank <- get_points(left_nodes, x_new)
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

#' Flip a 2D cross section
#'
#' Flips a 2D cross section so that the left side becomes the right,
#' and the right becomes the left.
#'
#' @param xs2d Single 2D cross section
#' @returns The original 2D cross section with the left and right
#' sides switched. The distance values (along the cross section) are
#' flipped in sign.
#' @export
flip_xs2d <- function(xs2d) {
  names(xs2d) <- rev(names(xs2d))
  xs2d$left$multiline[, 1]  <- -xs2d$left$multiline[, 1]
  xs2d$left$bank[1]         <- -xs2d$left$bank[1]
  xs2d$left$thalweg[1]      <- -xs2d$left$thalweg[1]
  xs2d$right$multiline[, 1] <- -xs2d$right$multiline[, 1]
  xs2d$right$bank[1]        <- -xs2d$right$bank[1]
  xs2d$right$thalweg[1]     <- -xs2d$right$thalweg[1]
  xs2d
}

#' Exaggerate Relief of 2D Cross Section
#'
#' Sometimes it's hard to see vertical relief in a plot of a 2D cross
#' section. This function exaggerates the relief by stretching it
#' by a multiplicative factor.
#'
#' @param xs2d A single 2D cross section object.
#' @param times Multiplier to exaggerate the relief by. Single positive
#' numeric. Numbers >1 will stretch the relief; <1 will compress.
#' @returns The original cross section, with exaggerated elevations
#' (according to height above thalweg).
#' @export
exaggerate_relief <- function(xs2d, times = 1) {
  checkmate::assert_numeric(times, 0, len = 1)
  ymin <- xs2d$left$thalweg[2]
  xs2d$left$multiline[, 2]  <- ymin + times * (xs2d$left$multiline[, 2] - ymin)
  xs2d$left$bank[2]         <- ymin + times * (xs2d$left$bank[2] - ymin)
  xs2d$left$thalweg[2]      <- ymin + times * (xs2d$left$thalweg[2] - ymin)
  xs2d$right$multiline[, 2] <- ymin + times * (xs2d$right$multiline[, 2] - ymin)
  xs2d$right$bank[2]        <- ymin + times * (xs2d$right$bank[2] - ymin)
  xs2d$right$thalweg[2]     <- ymin + times * (xs2d$right$thalweg[2] - ymin)
  xs2d
}

#' @rdname xt_widen_2d
#' @export
xt_widen_right_2d <- function(xs, dw) {
  checkmate::assert_numeric(dw, 0, len = 1)
  if (dw == 0) return(xs)
  xs <- flip_xs2d(xs)
  xs <- st_widen_left_2d(xs, dw)
  flip_xs2d(xs)
}

#' @rdname xt_widen_2d
#' @export
xt_widen_2d <- function(xs, dw, prop_left = 0.5) {
  checkmate::assert_numeric(dw, 0, len = 1)
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  dw_left <- prop_left * dw
  dw_right <- dw - dw_left
  xs <- xt_widen_right_2d(xs, dw_right)
  xt_widen_left_2d(xs, dw_left)
}
