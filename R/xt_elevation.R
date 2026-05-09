#' Get elevation values using an elevation specification
#'
#' @param channel An [`xchan_tbl`][xt_as_channel()], [`xchan`][xchan()], or [`xsection`][xsection()]
#'   object. Single-section inputs are wrapped for evaluation.
#' @param reference An elevation specification from [elevation_thalweg()] and related
#'   helpers (class `"xchan_elevation"`).
#' @param ... Reserved for methods (must be empty).
#' @returns A numeric vector of elevations, one per cross section (length `nrow()` for
#'   tables, `length()` for [`xchan`], or `1` for a single [`xsection`]).
#'
#' **Order:** Values follow **storage order only** — position `i` is always row `i` of the
#' channel table (or `[[i]]` of the [`xchan()`] list). There is **no** sorting inside
#' `xt_elevation()`. Reorder the table first (e.g. [xt_arrange_downstream()] for increasing
#' chainage along the axis from its current start; after [xt_reverse_flow()], the stored axis
#' is reversed so that sort follows hydrologic downstream).
#'
#' @details
#' Elevation specifications read each cross section's profile from the channel table or
#' geometry container; profile geometry must exist when the chosen reference requires it.
#'
#' @examples
#' # Get thalweg elevations (channel table)
#' elevations <- xt_elevation(channel, reference = elevation_thalweg())
#'
#' # Same elevations from the `xchan` geometry column
#' xc <- channel[[attr(channel, "xsection_col", exact = TRUE)]]
#' elevations <- xt_elevation(xc, reference = elevation_thalweg())
#'
#' # One section
#' xs <- xc[[1]]
#' z <- xt_elevation(xs, reference = elevation_thalweg())
#' @export
xt_elevation <- function(channel, reference, ...) {
  rlang::check_dots_empty()
  UseMethod("xt_elevation")
}

#' @rdname xt_elevation
#' @export
xt_elevation.xchan_tbl <- function(channel, reference, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan_tbl")
  reference(channel)
}

#' @rdname xt_elevation
#' @export
xt_elevation.xchan <- function(channel, reference, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")
  reference(xt_as_channel(channel))
}

#' @rdname xt_elevation
#' @export
xt_elevation.xsection <- function(channel, reference, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xsection")
  reference(xt_as_channel(xchan(list(channel))))
}

#' @rdname xt_elevation
#' @exportS3Method xt_elevation default
xt_elevation.default <- function(channel, reference, ...) {
  stop(
    "No `xt_elevation()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan_tbl`, `xchan`, or `xsection` object.",
    call. = FALSE
  )
}
