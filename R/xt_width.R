#' Width of cross sections
#'
#' `xt_width()` returns geometric width. For a \code{xchan} object, this is typically one
#' value per cross section: from planimetric line lengths when a plan column
#' exists, otherwise from profile geometry. For a single \code{xs_profile}
#' object, it is the span along the profile horizontal axis between the
#' outermost left and right banks (the same convention as
#' [xt_generate_profile()] and [xt_profile()]).
#'
#' @param x A `xchan` or `xs_profile` object.
#' @param ... Unused (reserved for methods).
#'
#' @returns
#' For `xchan`: a numeric vector with one width per row.
#' For `xs_profile`: a non-negative numeric scalar.
#'
#' @examples
#' library(sf)
#' seg <- st_sfc(
#'   st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
#'   crs = 3005
#' )
#' coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
#' xs <- xt_profile(coords, bankpoints = c(-1, 1))
#' xt_width(xs)
#'
#' # xt_width(demo_channel)
#' @export
#' @rdname widths
xt_width <- function(x, ...) {
  UseMethod("xt_width")
}

#' @export
#' @rdname widths
xt_width.xchan <- function(x, ...) {
  checkmate::assert_class(x, "xchan")
  if (xt_has_plan(x)) {
    plan <- xt_column_plan(x)
    return(vapply(plan, function(g) as.numeric(sf::st_length(g)), numeric(1)))
  }
  prof <- xt_column_profile(x)
  vapply(prof, xt_width, FUN.VALUE = numeric(1))
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
    ". Use a `xchan` or `xs_profile` object.",
    call. = FALSE
  )
}

#' @noRd
xt_validate_plan_profile_widths <- function(channel, tol = 1e-6) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_number(tol, lower = 0)
  if (!xt_has_plan(channel) || !xt_has_profile(channel)) {
    return(invisible(channel))
  }
  plan <- xt_column_plan(channel)
  profile <- xt_column_profile(channel)
  if (length(plan) != length(profile)) {
    stop(
      "Planimetric and profile columns must have the same length (got ",
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
