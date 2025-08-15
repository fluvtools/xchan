#' Get Channel Width
#'
#' @param object Cross section.
#' @returns Cross section width; single numeric.
#' @examples
#' xs <- xt_sxc(1:3)
#' xt_width(xs)
#' @rdname xt_width
#' @export
xt_width <- function(object) UseMethod("xt_width")

#' @export
xt_width.sf <- function(object) {
  xs <- sf::st_geometry(object)
  if (!is_sxc(xs)) {
    stop(
      "The geometry column in the inputted sf object is not a cross section ",
      "object set (class 'sxc')."
    )
  }
  xt_width(xs)
}

#' @export
xt_width.sxc <- function(object) {
  class(object) <- class(object)[-1] # sf weirdness
  sf::st_length(object)
}

#' @export
xt_width.sxc_2d <- function(xs) {
  xs$right$bank[1] - xs$left$bank[1]
}
