#' @export
xt_height_lb <- function(xs) {
  max(xs$left$bank[2] - xs$left$thalweg[2], 0)
}

#' @export
xt_height_rb <- function(xs) {
  max(xs$right$bank[2] - xs$right$thalweg[2], 0)
}
