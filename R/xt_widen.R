#' Widen cross sections
#'
#' @param channel An [`xchan`] or [`xsection`] object.
#' @param ... Must be empty (named `dw` / `dv` arguments are required).
#' @param dw The total width to add to the channel. Positive numeric, or a
#'   [units::units()] length object (for example
#'   `units::set_units(2, "m")`); units are converted to the channel's CRS
#'   length unit. Cannot be used with `dv`.
#' @param dv The total volume to remove to widen the channel. Positive numeric,
#'   or a [units::units()] volume object (for example
#'   `units::set_units(50, "m^3")`); units are converted to the channel's CRS
#'   length unit cubed. Cannot be used with `dw`.
#' @param side A side specification controlling how widening is split between
#'   left and right banks. Supply either a side object from [side_left()],
#'   [side_right()], or [side_both()], or a shorthand string: `"left"`,
#'   `"right"`, or `"both"`.
#' @note
#' The ellipsis `...` must be empty; named `dw` and `dv` keep widening deliberate.
#' @returns Object of the same class as `channel`, with widened sections.
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

  unit <- crs_length_unit(channel)

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
      do.call(
        widen_profile,
        list(
          profile[[i]],
          dw[i],
          prop_left[i]
        )
      )
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
