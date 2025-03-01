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
xt_widen_by <- function(object, by, prop_left = 0.5) UseMethod("xt_widen_by")

#' @export
xt_widen_by.sxc <- function(object, by, prop_left = 0.5) {
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  by_left <- by * prop_left
  by_right <- by - by_left
  object <- xt_widen_by_right(object, by = by_right)
  object <- xt_widen_by_right(flip_sxc(object), by = by_left)
  flip_sxc(object)
}

#' @export
xt_widen_by_right <- function(sxc, by) {
  checkmate::assert_class(sxc, "sxc")
  checkmate::assert_numeric(by)
  by <- vctrs::vec_recycle(by, length(sxc))
  one <- sf::st_length(sf::st_sfc(sxc)[1])
  one <- one / units::set_units(one, NULL)
  if (all(units::set_units(by, NULL) == 0)) {
    return(sxc)
  }
  for (i in seq_along(sxc)) {
    mat <- sxc[[i]]
    n <- nrow(mat)
    base_pt <- mat[1, 1:2]        # left
    end_pt <- mat[n, 1:2] # right
    vec <- end_pt - base_pt
    mag <- sqrt(sum(vec^2))
    unit_vec <- vec / mag
    translation_vec <- unit_vec * by[i]
    sxc[[i]][n, 1:2] <- units::set_units(
      sxc[[i]][n, 1:2] * one + translation_vec, NULL
    )
  }
  # vector <- sf::st_sfc(sxc)
  # mags <- sf::st_length(vector)
  # base_pts <- lapply(vector, \(v) {
  #   sf::st_point(sf::st_coordinates(v)[1, 1:2, drop = FALSE])
  # })
  # base_pts <- sf::st_sfc(base_pts)
  # unit_vec <- (vector - base_pts) / mags
  # translation_vec <- unit_vec * by
  # translation_pt <- lapply(translation_vec, \(v) {
  #   mat <- sf::st_coordinates(v)
  #   n <- nrow(mat)
  #   sf::st_point(mat[n, 1:2, drop = FALSE])
  # })
  # for (i in 1:length(sxc)) {
  #   n <- nrow(sxc[[i]])
  #   sxc[[i]][n, 1:2] <- sxc[[i]][n, 1:2] + translation_pt[[i]]
  # }
  sxc
}

#' @export
xt_widen_by.sf <- function(object, by, prop_left = 0.5) {
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  checkmate::assert_numeric(by)
  xs <- sf::st_geometry(object)
  if (!is_sxc(xs)) {
    stop(
      "The geometry column in the inputted sf object is not a cross section ",
      "object set (class 'sxc')."
    )
  }
  wider <- xt_widen_by(xs, by = by, prop_left = 0.5)
  sf::st_geometry(object) <- wider
  object
}
