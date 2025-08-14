#' Get the number of cross sections
#'
#' A generic function to determine the number of cross sections in an object.
#'
#' @param x A cross sections object of class `sx`, `sx_1d`, or `sx_2d`.
#' @param ... Additional arguments passed to methods.
#' @return A single integer representing the number of cross sections.
#' @export
xt_n_sections <- function(x, ...) {
  UseMethod("xt_n_sections")
}

#' @export
xt_n_sections.sx_1d <- function(x, ...) {
  ellipsis::check_dots_empty()
  length(x)
}

#' @export
xt_n_sections.sx_2d <- function(x, ...) {
  ellipsis::check_dots_empty()
  length(x)
}

#' @export
xt_n_sections.sx <- function(x, ...) {
  ellipsis::check_dots_empty()
  nrow(x)
}
