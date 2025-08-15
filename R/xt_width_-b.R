#' @export
xt_width_lb <- function(xs) {
  xs$left$thalweg[1] - xs$left$bank[1]
}

#' @export
xt_width_rb <- function(xs) {
  xs$right$bank[1] - xs$right$thalweg[1]
}

