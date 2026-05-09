#' Widen cross sections
#'
#' @param channel A `xchan_tbl`, `xchan`, or `xsection` object.
#' @param ... Additional arguments (ignored)
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
#' While the ellipsis `...` is currently not used, it forces the `dw` and
#' `dv` arguments to be named to ensure deliberate specification.
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
xt_widen.xchan_tbl <- function(
  channel,
  ...,
  dw,
  dv,
  side = "both"
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan_tbl")

  if (!xor(missing(dw), missing(dv))) {
    stop("Must specify either `dw` or `dv`, but not both.")
  }

  # Get derived plan and profile views
  plan <- channel_plan(channel)
  profile <- channel_profile(channel)

  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)
  n_sections <- xt_n_sections(channel)

  # Strip user-supplied units (if any) into the channel's CRS unit so that
  # downstream coordinate arithmetic stays plain numeric.
  unit <- crs_length_unit(channel)

  # Get dw if dv was provided
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
    # Avoid 0 / 0 when neither side eroded.
    denom <- dw_left + dw_right
    prop_left <- ifelse(denom > 0, dw_left / denom, 0.5)
    dw <- dw_left + dw_right
  } else {
    dw <- to_numeric_length(dw, unit, arg = "dw")
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

  # Update xsection geometry once, then validate once.
  xcol <- attr(channel, "xsection_col", exact = TRUE)
  if (is.null(xcol) || !xcol %in% names(channel)) {
    stop("Channel has no `xsection` geometry column.", call. = FALSE)
  }
  xsec <- channel[[xcol]]
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
  channel[[xcol]] <- xsec
  validate_plan_profile_widths(channel)

  channel
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
  out <- xt_widen(xt_as_channel(channel), dw = dw, dv = dv, side = side)
  xcol <- attr(out, "xsection_col", exact = TRUE)
  out[[xcol]]
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
    ". Use a `xchan_tbl`, `xchan`, or `xsection` object.",
    call. = FALSE
  )
}
