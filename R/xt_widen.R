#' Widen cross sections
#'
#' @param channel An [`xchan`] or [`xsection`] object.
#' @param ... Must be empty (named `dw` / `dv` arguments are required).
#' @param dw The total width to add to the channel. Positive numeric, or a
#'   [units::units()] length object (for example `units::set_units(2, "m")`);
#'   units are converted to the channel's CRS length unit. Cannot be used with
#'   `dv`.
#' @param dv The total volume to remove to widen the channel. Positive numeric,
#'   or a [units::units()] volume object (for example `units::set_units(50,
#'   "m^3")`); units are converted to the channel's CRS length unit cubed.
#'   Cannot be used with `dw`.
#' @param side A side specification controlling how widening is split between
#'   left and right banks. Supply either a side object from [side_left()],
#'   [side_right()], or [side_both()], or a shorthand string: `"left"`,
#'   `"right"`, or `"both"`.
#' @note
#' The ellipsis `...` must be empty; named `dw` and `dv` keep widening
#' deliberate.
#'
#' @details
#' The stored channel axis ([xt_axis()]) is **not** updated when widening: plan
#' and profile transects move, but the reach-scale axis polyline is left
#' unchanged. If you set the axis to something tied to the pre-widen plan (for
#' example a digitized centerline), do not expect it to refit automatically to a
#' new midline---that is intentional in most workflows, because the axis is used
#' for cross-section ordering and downstream metrics ([xt_arrange_downstream()],
#' [xt_distance_downstream()], etc.) rather than as a moving geometric center of
#' each transect. To install a different axis, use the replacement form
#' `xt_axis(channel) <- value` (see [xt_axis()]).
#'
#' Profile distances are re-centered after widening so that `distance = 0`
#' remains the midpoint of the outer-bank pair.
#' @returns Object of the same class as `channel`, with widened sections.
#' @examples
#' channel <- xt_as_channel(rep(1, 6))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' xt_widen(channel, dw = 10)
#' xt_widen(channel, dw = 10, side = side_left(0.75))
#' xt_widen(channel, dv = 5, side = "right")
#' @export
xt_widen <- function(
  channel,
  ...,
  dw,
  dv,
  side = "both"
) {
  UseMethod("xt_widen")
}

#' @export
#' @rdname xt_widen
xt_widen.xchan <- function(
  channel,
  ...,
  dw,
  dv,
  side = "both"
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")

  if (!xor(missing(dw), missing(dv))) {
    stop("Must specify either `dw` or `dv`, but not both.")
  }

  plan <- channel_plan(channel)
  profile <- channel_profile(channel)

  prop_left <- parse_side_arg(side, channel)
  n_sections <- xt_n_sections(channel)

  unit <- channel_length_unit(channel)

  if (missing(dw)) {
    if (is.null(profile)) {
      stop(
        "Channel object must have profile cross sections to calculate ",
        "widening from volume."
      )
    }
    dv <- to_numeric_volume(dv, unit, arg = "dv")
    dw_left <- erosion_width_numeric(
      channel,
      dv * prop_left,
      side = "left"
    )
    dw_right <- erosion_width_numeric(
      channel,
      dv * (1 - prop_left),
      side = "right"
    )
    denom <- dw_left + dw_right
    prop_left <- ifelse(denom > 0, dw_left / denom, 0.5)
    dw <- dw_left + dw_right
  } else {
    dw <- to_numeric_length(dw, unit, arg = "dw")
  }
  dw <- vctrs::vec_recycle(dw, n_sections)
  prop_left <- vctrs::vec_recycle(prop_left, n_sections)

  if (!is.null(plan)) {
    plan <- widen_plan(plan, dw = dw, prop_left = prop_left)
  }

  if (!is.null(profile)) {
    profile <- lapply(seq_along(profile), function(i) {
      prof_i <- profile[[i]]
      left0 <- get_left_bank_coords(prof_i)[1]
      right0 <- get_right_bank_coords(prof_i)[1]
      widened <- do.call(
        widen_profile,
        list(
          prof_i,
          dw[i],
          prop_left[i]
        )
      )
      widened <- snap_profile_bank_positions(
        widened,
        left_x = left0 - dw[i] * prop_left[i],
        right_x = right0 + dw[i] * (1 - prop_left[i])
      )
      recenter_profile_distances(widened)
    })
  }

  xsec <- channel
  if (!is.null(plan)) {
    vp <- validate_plan(plan)
    if (!vp$valid) {
      stop(
        "Invalid plan after widening: ",
        paste(vp$issues, collapse = "; "),
        call. = FALSE
      )
    }
    xsec <- xchan_with_plan(xsec, plan)
  }
  if (!is.null(profile)) {
    xsec <- xchan_with_profile(xsec, profile)
  }
  validate_plan_profile_widths(xsec)

  xsec
}

#' @export
#' @rdname xt_widen
xt_widen.xsection <- function(
  channel,
  ...,
  dw,
  dv,
  side = "both"
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xsection")
  wrapped <- xchan(list(channel), crs = sf::NA_crs_)
  xt_widen(wrapped, dw = dw, dv = dv, side = side)[[1L]]
}

#' @exportS3Method xt_widen default
xt_widen.default <- function(
  channel,
  ...,
  dw,
  dv,
  side = "both"
) {
  stop(
    "No `xt_widen()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan` or `xsection` object.",
    call. = FALSE
  )
}
