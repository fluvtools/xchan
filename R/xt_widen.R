#' Widen a channel
#'
#' @param channel Channel object
#' @param ... Additional arguments (ignored)
#' @param dw The total width to add to the channel. Must be a positive numeric
#'   value and cannot be used with `dv`.
#' @param dv The total volume to remove to widen the channel. Must be a
#'   positive numeric value and cannot be used with `dw`.
#' @param side A side specification controlling how widening is split between
#'   left and right banks. Supply either a side object from [side_left()],
#'   [side_right()], or [side_both()], or a shorthand string: `"left"`,
#'   `"right"`, or `"both"`.
#' @param on_overflow What to do if the widening exceeds the cross section
#'   extent; either "error" (the default), or "repeat", which will repeat the
#'   widening with the last available topography elevation.
#' @note
#' While the ellipsis `...` is currently not used, it forces the `dw` and
#' `dv` arguments to be named to ensure deliberate specification.
#' @returns A modified channel object
#' @examples
#' xt_widen(channel, dw = 10)
#' xt_widen(channel, dw = 10, side = side_left(0.75))
#' xt_widen(channel, dv = 5, side = "right")
#' @export
xt_widen <- function(
  channel,
  ...,
  dw,
  dv,
  side = "both",
  on_overflow = c("error", "repeat")
) {
  on_overflow <- rlang::arg_match(on_overflow)
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")

  if (!xor(missing(dw), missing(dv))) {
    stop("Must specify either `dw` or `dv`, but not both.")
  }

  # Get plan and profile columns
  plan <- xt_column_plan(channel)
  profile <- xt_column_profile(channel)

  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)
  n_sections <- xt_n_sections(channel)

  # Get dw if dv was provided
  if (missing(dw)) {
    if (is.null(profile)) {
      stop(
        "Channel object must have profile cross sections to calculate ",
        "widening from volume."
      )
    }
    dw_left <- xt_erosion_width(
      channel,
      dv * prop_left,
      side = "left",
      error_on_overflow = error_on_overflow
    )
    dw_right <- xt_erosion_width(
      channel,
      dv * (1 - prop_left),
      side = "right",
      error_on_overflow = error_on_overflow
    )
    # Avoid 0 / 0 when neither side eroded.
    denom <- dw_left + dw_right
    prop_left <- ifelse(denom > 0, dw_left / denom, 0.5)
    dw <- dw_left + dw_right
  }
  dw <- vctrs::vec_recycle(dw, n_sections)
  prop_left <- vctrs::vec_recycle(prop_left, n_sections)

  # Apply widening to planimetric cross-sections
  if (!is.null(plan)) {
    plan <- widen_plan(plan, dw = dw, prop_left = prop_left)
  }

  # Apply widening to profile cross-sections
  if (!is.null(profile)) {
    profile <- lapply(seq_along(profile), function(i) {
      widen_profile(profile[[i]], dw = dw[i], prop_left = prop_left[i])
    })
  }

  # Update the channel
  if (!is.null(plan)) {
    xt_column_plan(channel) <- plan
  }
  if (!is.null(profile)) {
    xt_column_profile(channel) <- profile
  }

  channel
}
