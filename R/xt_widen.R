#' Widen a channel
#'
#' @param channel Channel object
#' @param ... Additional arguments (ignored)
#' @param dw The total width to add to the channel. Must be a numeric value
#'   and cannot be used with `dv`.
#' @param dv The total volume to remove to widen the channel. Must be a
#'   numeric value and cannot be used with `dw`.
#' @param side A specification for how to distribute the widening between
#' left and right banks. Built-in side functions include "left", "right", and "both".
#'
#' A side object is a function of channels that determines the amount
#' of erosion occurring on the left bank (by convention) for each cross section.
#' Existing schemes have the naming convention `side_<name>` and are
#' determined by parameters. You can call them
#' directly, e.g. `side_left(0.75)`, or by their name using the default
#' parameters.
#' @note
#' While the ellipsis `...` is currently not used, it forces the `dw` and
#' `dv` arguments to be named to ensure deliberate specification.
#' @returns A modified channel object
#' @examples
#' xt_widen(channel, dw = 10)
#' xt_widen(channel, dw = 10, side = side_left(0.75))
#' xt_widen(channel, dv = 5, side = "right")
#' @export
xt_widen <- function(channel, ..., dw, dv, side = "both") {
  ellipsis::check_dots_empty()
  checkmate::assert_class(channel, "sxchan")

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
      side = "left"
    )
    dw_right <- xt_erosion_width(
      channel,
      dv * (1 - prop_left),
      side = "right"
    )
    prop_left <- dw_left / (dw_left + dw_right)
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

