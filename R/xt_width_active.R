#' Active cross-section widths (water-filled span)
#'
#' The **active** width is the portion of the cross section occupied by water
#' (not dry bars or islands): the sum of **water** intervals between consecutive
#' bank contacts. With profile geometry, bank positions come from the profile
#' (see [xt_add_profile()] and `xs_profile`). With **planimetric geometry only**,
#' every vertex of the plan polyline is treated as a bank contact along the
#' transect, in order from left bank to right bank, alternating water / land /
#' water (even vertex count). For a simple two-vertex bank-to-bank segment there
#' are no islands and the active width equals [xt_width()].
#'
#' @param channel An [`xchan`], an [`xsection`], or an `xs_profile`.
#' @returns
#' For [`xchan`]: a numeric vector, one value per cross section. When the channel
#'   has a defined length unit (CRS or manually set), values carry
#'   [units::units()] like [xt_width()]; otherwise plain numeric.
#' For [`xsection`] or `xs_profile`: a single non-negative numeric (plain
#'   numeric unless the [`xsection`] carries a `"crs"` attribute with a linear
#'   unit, in which case units may be attached).
#' @seealso [xt_width()]
#' @export
xt_width_active <- function(channel) {
  UseMethod("xt_width_active")
}

#' @export
xt_width_active.xchan <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  plan <- channel_plan(channel)
  profile <- channel_profile(channel)
  if (!is.null(profile)) {
    raw <- vapply(profile, active_width_from_profile, FUN.VALUE = numeric(1))
  } else {
    raw <- vapply(
      seq_along(plan),
      function(i) {
        xy <- sf::st_coordinates(plan[[i]])[, 1L:2L, drop = FALSE]
        active_width_from_plan_vertices(xy)
      },
      FUN.VALUE = numeric(1)
    )
  }
  with_length_units(raw, channel_length_unit(channel))
}

#' @export
xt_width_active.xs_profile <- function(channel) {
  checkmate::assert_class(channel, "xs_profile")
  active_width_from_profile(channel)
}

#' @export
xt_width_active.xsection <- function(channel) {
  checkmate::assert_class(channel, "xsection")
  p <- channel$profile
  if (!is.null(p)) {
    raw <- active_width_from_profile(p)
  } else {
    raw <- active_width_from_plan_vertices(channel$plan)
  }
  cr <- attr(channel, "crs", exact = TRUE)
  unit <- NULL
  if (!is.null(cr)) {
    crs_use <- if (inherits(cr, "crs")) {
      cr
    } else {
      suppressWarnings(sf::st_crs(cr))
    }
    if (inherits(crs_use, "crs") && !is.na(crs_use)) {
      g <- sf::st_sfc(xsection_to_linestring(channel), crs = crs_use)
      unit <- crs_length_unit(g)
    }
  }
  with_length_units(raw, unit)
}

#' @export
xt_width_active.default <- function(channel) {
  stop(
    "No `xt_width_active()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan`, `xsection`, or `xs_profile` object.",
    call. = FALSE
  )
}

#' @noRd
active_width_from_profile <- function(xs) {
  checkmate::assert_class(xs, "xs_profile")
  active_width_from_bank_distances(get_bank_distances(xs))
}

#' @noRd
active_width_from_bank_distances <- function(bank_d) {
  if (length(bank_d) %% 2L != 0L) {
    stop(
      "Expected an even number of bank points along the transect (got ",
      length(bank_d),
      ").",
      call. = FALSE
    )
  }
  w_water <- 0
  for (j in seq_len(length(bank_d) / 2L)) {
    w_water <- w_water + (bank_d[2L * j] - bank_d[2L * j - 1L])
  }
  w_water
}

#' @noRd
active_width_from_plan_vertices <- function(plan) {
  checkmate::assert_matrix(plan, min.rows = 2L, ncols = 2L, mode = "numeric")
  n <- nrow(plan)
  if (n %% 2L != 0L) {
    stop(
      "Planimetric transect must have an even number of vertices so bank ",
      "contacts alternate water / land / water along the line (got ",
      n,
      ").",
      call. = FALSE
    )
  }
  dxy <- diff(plan[, 1L])^2 + diff(plan[, 2L])^2
  seg <- sqrt(pmax(0, dxy))
  bank_d <- c(0, cumsum(seg))
  active_width_from_bank_distances(bank_d)
}
