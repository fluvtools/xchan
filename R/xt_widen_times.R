#' @rdname xt_widen
#' @param times Multiplicative factor to widen the channel by. Values less than
#' 1 will narrow cross sections. Either
#' a vector of length equal to the number of cross sections, or length 1.
#' @author Vincenzo Coia, Heba Abdelmoaty
#' @export
xt_widen_times <- function(
    object, times, side = c("both", "left", "right")
) UseMethod("xt_widen_times")

#' @export
xt_widen_times.sf <- function(object, times, side = c("both", "left", "right")) {
  xs <- sf::st_geometry(object)
  if (!is_sxc(xs)) {
    stop(
      "The geometry column in the inputted sf object is not a cross section ",
      "object set (class 'sxc')."
    )
  }
  wider <- xt_widen_times(xs, times = times, side = side)
  sf::st_geometry(object) <- wider
  object
}

#' @export
xt_widen_times.sxc <- function(object, times, side = c("both", "left", "right")) {
  side <- match.arg(side)
  n <- length(object)
  times <- vctrs::vec_recycle(times, n)
  times <- units::drop_units(times)
  for (i in seq_len(n)) {
    xs <- object[[i]]
    coords <- st_coordinates(xs)
    if (nrow(coords) != 2) {
      stop("Each cross-section must be a LINESTRING with exactly two points.")
    }
    # Define the reference point based on the selected side
    if (side == "left") {
      reference_point <- coords[2, 1:2]  # Use left point as reference
    } else if (side == "right") {
      reference_point <- coords[1, 1:2]  # Use right point as reference
    } else {
      reference_point <- st_coordinates(sf::st_centroid(xs))[, 1:2]
    }
    object[[i]] <- (xs - reference_point) * times[i] + reference_point
  }
  new_sxc(sf::st_sfc(object, recompute_bbox = TRUE))
}
