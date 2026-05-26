#' Width of cross sections
#'
#' `xt_width()` returns geometric width. For an [`xchan`], this is one value per
#' cross section from planimetric line lengths. For an [`xsection`], it is the
#' length of that plan polyline (the same value as the corresponding element of
#' [xt_width()] on the parent channel). For a single `xs_profile` object, it is
#' the span along the profile horizontal axis between the outermost left and
#' right banks (the same convention as [xt_generate_profile()] and
#' [xt_add_profile()]).
#'
#' @param x An [`xchan`], [`xsection`], or `xs_profile` object.
#' @param ... Unused (reserved for methods).
#'
#' @returns
#' For [`xchan`]: a numeric vector with one width per cross section, carrying
#'   [units::units()] when the channel has a CRS with a defined linear unit
#'   (for example metres), or when a length unit was set manually (for example
#'   via [units::units()] widths or profile distances). When no unit is known
#'   the result is plain numeric.
#' For [`xsection`]: a non-negative numeric scalar. If attribute `"crs"` is
#'   set on `x` (unusual; the container [`xchan`] holds CRS instead), the result
#'   may carry [units::units()] like a channel with that CRS; otherwise plain
#'   numeric.
#' For `xs_profile`: a non-negative numeric scalar (no CRS context, so plain
#'   numeric).
#'
#' @examples
#' library(sf)
#' seg <- st_sfc(
#'   st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
#'   crs = 3005
#' )
#' coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
#' xs <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
#' xt_width(xs)
#'
#' # xt_width(Squamish_channel)
#' @export
#' @rdname widths
xt_width <- function(x, ...) {
  UseMethod("xt_width")
}

#' @export
#' @rdname widths
xt_width.xchan <- function(x, ...) {
  checkmate::assert_class(x, "xchan")
  plan <- channel_plan(x)
  raw <- vapply(plan, function(g) as.numeric(sf::st_length(g)), numeric(1))
  with_length_units(raw, channel_length_unit(x))
}

#' @export
#' @rdname widths
xt_width.xsection <- function(x, ...) {
  checkmate::assert_class(x, "xsection")
  g <- xsection_to_linestring(x)
  cr <- attr(x, "crs", exact = TRUE)
  unit <- NULL
  raw <- if (!is.null(cr)) {
    crs_use <- if (inherits(cr, "crs")) {
      cr
    } else {
      suppressWarnings(sf::st_crs(cr))
    }
    if (inherits(crs_use, "crs") && !is.na(crs_use)) {
      g <- sf::st_sfc(g, crs = crs_use)
      unit <- crs_length_unit(g)
      as.numeric(sf::st_length(g))
    } else {
      as.numeric(sf::st_length(g))
    }
  } else {
    as.numeric(sf::st_length(g))
  }
  with_length_units(raw, unit)
}

#' @export
#' @rdname widths
xt_width.xs_profile <- function(x, ...) {
  checkmate::assert_class(x, "xs_profile")
  unname(get_right_bank_coords(x)[1] - get_left_bank_coords(x)[1])
}

#' @exportS3Method xt_width default
xt_width.default <- function(x, ...) {
  stop(
    "No `xt_width()` method for class ",
    paste(class(x), collapse = "/"),
    ". Use an `xchan`, `xsection`, or `xs_profile` object.",
    call. = FALSE
  )
}

#' @noRd
validate_plan_profile_widths <- function(channel, tol = 1e-6) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_number(tol, lower = 0)
  if (!xt_has_profile(channel)) {
    return(invisible(channel))
  }
  plan <- channel_plan(channel)
  profile <- channel_profile(channel)
  if (length(plan) != length(profile)) {
    stop(
      "Planimetric and profile views must have the same length (got ",
      length(plan), " and ", length(profile), ").",
      call. = FALSE
    )
  }
  plan_w <- vapply(plan, function(g) as.numeric(sf::st_length(g)), numeric(1))
  prof_w <- vapply(profile, xt_width, FUN.VALUE = numeric(1))
  diff <- abs(plan_w - prof_w)
  bad <- diff > tol
  if (any(bad)) {
    idx <- which(bad)
    details <- paste(
      sprintf(
        "section %s: plan width %.10g, profile width %.10g (diff %.10g)",
        idx,
        plan_w[idx],
        prof_w[idx],
        diff[idx]
      ),
      collapse = "; "
    )
    stop(
      "Planimetric cross section length(s) disagree with profile width(s): ",
      details,
      ". Ensure each xs_profile spans the same distance as its plan line ",
      "(see `xt_width()` for both plan and profile objects).",
      call. = FALSE
    )
  }
  invisible(channel)
}
