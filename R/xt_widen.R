#' Widen a channel
#'
#' @param channel Channel object
#' @param ... Additional arguments (ignored)
#' @param width The total width to add to the channel. Must be a numeric value
#'   and cannot be used with `volume`.
#' @param volume The total volume to remove to widen the channel. Must be a
#'   numeric value and cannot be used with `width`.
#' @param side A specification for how to distribute the widening between
#' left and right banks. Built-in splitters include "left", "right", and "both".
#'
#' A splitter object is a function of channels that determines the amount
#' of erosion occurring on the left bank (by convention) for each cross section.
#' Existing schemes have the naming convention `splitter_<name>` and are
#' determined by parameters. You can call them
#' directly, e.g. `splitter_left(0.75)`, or by their name using the default
#' parameters. You can create your own splitter with `new_splitter()`.
#' @note
#' While the ellipsis `...` is currently not used, it forces the `width` and
#' `volume` arguments to be named to ensure deliberate specification.
#' @returns A modified channel object
#' @examples
#' xt_widen(channel, width = 10)
#' xt_widen(channel, width = 10, side = splitter_left(0.75))
#' xt_widen(channel, volume = 5, side = "right")
#' @export
xt_widen <- function(channel, ..., width, volume, side = "both") {
  ellipsis::check_dots_empty()
  checkmate::assert_class(channel, "sxchan")
  
  if (!xor(missing(width), missing(volume))) {
    stop("Must specify either `width` or `volume`, but not both.")
  }
  
  # Get plan and profile columns
  plan <- xt_column_plan(channel)
  profile <- xt_column_profile(channel)
  
  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)
  
  # Apply widening to planimetric cross-sections
  if (!is.null(plan)) {
    if (!missing(width)) {
      plan <- xt_widen_width_1d(plan, by = width, prop_left = prop_left)
    } else {
      stop("Volume widening is not applicable to planimetric cross-sections")
    }
  }
  
  # Apply widening to profile cross-sections
  if (!is.null(profile)) {
    if (!missing(width)) {
      profile <- lapply(profile, function(xs) {
        xt_widen_width_2d(xs, dw = width, prop_left = prop_left)
      })
    } else {
      profile <- lapply(profile, function(xs) {
        xt_widen_volume_2d(xs, volume = volume, prop_left = prop_left)
      })
    }
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

