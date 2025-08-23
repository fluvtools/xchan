#' Reverse a Channel's Flow Direction
#'
#' Swaps what is called the "left" and "right" banks (from the perspective
#' of someone looking downstream).
#'
#' @param channel A cross section object.
#' @returns A cross section object with the flow direction reversed. That is,
#' what was previously called the left bank is now the right, and vice versa.
#' @details
#' Reversing a channel's flow does not change
#' the geometry of the plan view cross sections, but the way the
#' profile cross sections are encoded does change: the left and right parts
#' are flipped due to the convention that the left side of the profile
#' correspond to the left bank of the channel.
#' @export
xt_reverse_flow <- function(channel) {
  xt_column_plan(channel) <- flip_xs1d(xt_column_plan(channel))
  xt_column_profile(channel) <- flip_xs2d(xt_column_profile(channel))
  channel
}
