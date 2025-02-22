#' Widen cross sections
#'
#' Widen or narrow a collection of cross sections. `xt_widen_by()` widens
#' (narrows) by unit length; `xt_widen_times()` widens (narrows) by
#' a multiplicative factor.
#'
#' @param object A cross section object.
#' @param by Amount to widen the channel by, using units in common with the
#' cross sectional units. Negative values will narrow cross sections. Either
#' a vector of length equal to the number of cross sections, or length 1.
#' @param side Which side of the cross section to widen by? One of
#' `"both"` (splits the change in width equally between both sides),
#' `"left"`, or `"right"` (applies the width change to one side of the
#' channel).
#' @rdname xt_widen
#' @export
xt_widen_by <- function(object, by, side = c("both", "left", "right")) UseMethod("xt_widen_by")

#' @export
xt_widen_by.sf <- function(object, by, side = c("both", "left", "right")) {
  xs <- sf::st_geometry(object)
  if (!is_sxc(xs)) {
    stop(
      "The geometry column in the inputted sf object is not a cross section ",
      "object set (class 'sxc')."
    )
  }
  wider <- xt_widen_by(xs, by = by, side = side)
  sf::st_geometry(object) <- wider
  object
}

#' @export
xt_widen_by.sxc <- function(object, by, side = c("both", "left", "right")) {
  n <- length(object)
  by <- vctrs::vec_recycle(by, n)
  l <- xt_width(object)
  times <- (l + by) / l
  xt_widen_times(object, times = times, side = side)
}
