#' @export
xs_width <- function(xs) {
  xs$right$bank[1] - xs$left$bank[1]
}

#' @export
lb_width <- function(xs) {
  xs$left$thalweg[1] - xs$left$bank[1]
}

#' @export
rb_width <- function(xs) {
  xs$right$bank[1] - xs$right$thalweg[1]
}

