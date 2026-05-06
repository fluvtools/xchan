#' Reverse a Channel's Flow Direction
#'
#' Swaps what is called the "left" and "right" banks (from the perspective
#' of someone looking downstream).
#'
#' @param channel A cross section object.
#' @returns A cross section object with the flow direction reversed. That is,
#' what was previously called the left bank is now the right, and vice versa.
#' @details
#' Planimetric segments are reversed end-for-end (`flip_plan()`), so the first
#' vertex now corresponds to what was the right bank (and vice versa). Profile
#' cross sections, when present, are flipped with `flip_profile()` so signed
#' distances across the section stay aligned with the plan. When there is no
#' profile column, only the planimetric geometries are updated.
#' @note While the channel flow direction is reversed, the original order
#' of the rows in the `channel` data frame is preserved.
#' @export
xt_reverse_flow <- function(channel) {
  xt_column_plan(channel) <- flip_plan(xt_column_plan(channel))
  if (xt_has_profile(channel)) {
    profiles <- xt_column_profile(channel)
    xt_column_profile(channel) <- lapply(profiles, flip_profile)
  }
  channel
}
