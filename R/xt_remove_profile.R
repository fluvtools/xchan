#' Remove profile geometry from cross sections
#'
#' Drops `xs_profile` data from each [`xsection`] in an [`xchan`], or from a
#' single [`xsection`]. Planimetric geometry and channel CRS are unchanged.
#' Use [xt_as_sfc()] with `what = "profile"` when you need profile linestrings
#' as `sfc` before removing them.
#'
#' @param x An [`xchan`] or [`xsection`].
#' @param ... Must be empty.
#' @returns `x` with profile components set to `NULL`.
#' @examples
#' ch <- xt_as_channel(c(2, 2), crs = 3005)
#' xt_remove_profile(ch)
#' @export
xt_remove_profile <- function(x, ...) {
  UseMethod("xt_remove_profile")
}

#' @rdname xt_remove_profile
#' @export
xt_remove_profile.xchan <- function(x, ...) {
  rlang::check_dots_empty()
  out <- xchan_with_profile(x, NULL)
  validate_plan_profile_widths(out)
  out
}

#' @rdname xt_remove_profile
#' @export
xt_remove_profile.xsection <- function(x, ...) {
  rlang::check_dots_empty()
  x$profile <- NULL
  x
}

#' @rdname xt_remove_profile
#' @export
xt_remove_profile.default <- function(x, ...) {
  rlang::check_dots_empty()
  stop(
    "No `xt_remove_profile()` method for class ",
    paste(class(x), collapse = "/"),
    ".",
    call. = FALSE
  )
}
