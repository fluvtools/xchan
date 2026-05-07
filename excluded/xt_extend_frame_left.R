#' Extend topographic frame on one bank
#'
#' @description
#' `xt_extend_frame_left()` extends the profile cross-section beyond the
#' **left** bank only; `xt_extend_frame_right()` extends beyond the **right**
#' bank only. Each adds topography using [extender_flat()] /
#' [extender_slope()] (and related) operators so there is space for widening
#' or erosion calculations on that side.
#'
#' For symmetric extension on **both** banks in one step, see
#' [xt_extend_frame()].
#'
#' @param channel Channel object with profile cross sections.
#' @param extender Extender operator from the `extender_*()` family (e.g.
#'   [extender_flat()], [extender_slope()]).
#'
#' @returns A modified channel object whose profile frame is extended on the
#'   requested side (once implemented; currently returns `channel`
#'   unchanged).
#'
#' @details
#' Extenders are operator factories: they build functions that define how far
#' and how to extend topography beyond the bank. Side-specific functions apply
#' that extension only on the left or right half of each profile.
#'
#' @seealso [xt_extend_frame()] to extend both banks together.
#'
#' @examples
#' # Flat extension on the left bank only
#' channel <- xt_as_channel(c(10, 12, 8, 15, 11, 9))
#' channel <- xt_extend_frame_left(
#'   channel,
#'   extender = extender_flat(extent = 20)
#' )
#'
#' # Sloped extension on the right bank only
#' channel <- xt_extend_frame_right(
#'   channel,
#'   extender = extender_slope(extent = 30, slope = 0.02)
#' )
#'
#' @rdname xt_extend_frame_lr
#' @export
xt_extend_frame_left <- function(channel, extender) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_class(extender, "xchan_extender")

  # For now, return placeholder implementation
  # This will be implemented to extend only the left side
  channel
}
