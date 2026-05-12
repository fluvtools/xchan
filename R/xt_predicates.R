#' Predicate tests for channel geometry and cross sections
#'
#' \code{xt_is_channel()} is \code{TRUE} for the cross-section list container
#' (\code{xchan}). \code{xt_is_cross_section()}
#' tests for a single cross section (same check as [is.xsection()]).
#'
#' @name xt_is_channel
#' @param x Any object.
#' @returns `TRUE` or `FALSE`.
#'
#' @seealso [is.xchan], [is.xsection]
#'
#' @examples
#' \donttest{
#' xc <- xt_as_channel(c(10, 12, 11))
#' xt_is_channel(xc)
#' xt_is_cross_section(xc[[1]])
#' }
#'
#' @export
xt_is_channel <- function(x) {
  inherits(x, "xchan")
}

#' @rdname xt_is_channel
#' @export
xt_is_cross_section <- function(x) {
  is.xsection(x)
}
