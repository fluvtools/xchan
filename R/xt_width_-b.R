#' @param xs `xs_profile`; left/right half-width helpers below.
#' @export
#' @rdname widths
xt_width_lb <- function(xs) {
  xs$left$thalweg[1] - xs$left$bank[1]
}

#' @export
#' @rdname widths
xt_width_rb <- function(xs) {
  xs$right$bank[1] - xs$right$thalweg[1]
}
