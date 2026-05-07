#' Find the erosion width for given volume to the right of a bank point
#'
#' Computes the change in horizontal position to the right of a given bank point
#' `x0` such that the trapezoidal area (above the thalweg
#' height) equals the specified erosion volume `v`. The cross section is
#' defined by `topo`, a 2-column matrix of horizontal positions and ground
#' elevations.
#'
#' Heights below the thalweg elevation contribute zero to the area, so erosion
#' into depressions below the thalweg level does not reduce the accumulated
#' volume.
#'
#' @param v Numeric scalar. Target erosion volume (area in cross-section units)
#'           to the right of `x0`, relative to the thalweg elevation.
#' @param x0 Numeric scalar. The horizontal position of the starting bank point
#'            in the cross-section. Must exactly match one value in `topo[,1]`.
#' @param topo Numeric matrix with two columns: the first column contains
#'             horizontal positions (x-coordinates), and the second column
#'             contains ground elevations at those positions. Rows need not be
#'             sorted by x.
#' @param thalweg_height Numeric scalar. Elevation of the thalweg used as the
#'                       zero reference level for volume calculations.
#' @param valley Character string; volume exactly brings you to a point that
#'        goes below the thalweg, should the returned x value stop there
#'        ("left") or extend to the point where the elevation rises above
#'        the thalweg again ("right", the default)? Corresponds to the left and
#'        right sides of the valley, respectively.
#' @returns Numeric scalar giving the change in x-position to the right of `x0`
#'         where the cumulative trapezoidal area above the thalweg height
#'         equals `v`.
#'
#' @details
#' The function proceeds from `x0` to the right, summing trapezoidal areas
#' between consecutive cross-section points until the cumulative area reaches or
#' exceeds `v`. The final position is determined by solving exactly for the
#' location within the last trapezoid where the target volume is achieved.
#'
#' The method is fully vectorized and runs in \eqn{O(n)} time, where \eqn{n}
#' is the number of cross-section points to the right of `x0`.
#'
#' If the requested volume cannot be satisfied within the available topography
#' to the right of `x0`, the function throws an error rather than extrapolating
#' beyond the cross-section extent.
#'
#' For `v = 0`, the convention is adopted where `x0` is always returned, even
#' in the unusual case where the bank is below thalweg and `valley = "right"`.
#'
#' @examples
#' topo <- matrix(
#'   c(
#'     0, 10,
#'     1, 12,
#'     2, 15,
#'     3, 7,
#'     4, 11,
#'     5, 12
#'   ),
#'   ncol = 2,
#'   byrow = TRUE
#' )
#'
#' find_dx_for_volume_right(4.5, x0 = 1, topo = topo, thalweg_height = 9)
#' find_dx_for_volume_right(6, x0 = 1, topo = topo, thalweg_height = 9)
#'
#' # Floodplain goes below thalweg at x=2.5 until x=4
#' find_dx_for_volume_right(
#'   3.5, x0 = 1, topo = topo, thalweg_height = 11, valley = "right"
#' )
#' find_dx_for_volume_right(
#'   3.5, x0 = 1, topo = topo, thalweg_height = 11, valley = "left"
#' )
#' find_dx_for_volume_right(
#'   4, x0 = 1, topo = topo, thalweg_height = 11
#' )
#' find_dx_for_volume_right(
#'   4.001, x0 = 1, topo = topo, thalweg_height = 11
#' )
#'
#' # No volume to the right of x0 returns x0, unless in the unusual situation
#' # where the bank is at or below the thalweg height and valley = "right".
#' find_dx_for_volume_right(
#'   0, x0 = 1, topo = topo, thalweg_height = 11
#' )
#' find_dx_for_volume_right(
#'   0, x0 = 2.5, topo = topo, thalweg_height = 11, valley = "right"
#' )
#' find_dx_for_volume_right(
#'   0, x0 = 2.5, topo = topo, thalweg_height = 11, valley = "left"
#' )
find_dx_for_volume_right <- function(
  v,
  x0,
  topo,
  thalweg_height,
  valley = c("right", "left")
) {
  checkmate::assert_number(v, lower = 0)
  checkmate::assert_number(x0)
  checkmate::assert_matrix(topo, ncol = 2L, min.rows = 1L, mode = "numeric")
  checkmate::assert_number(thalweg_height)
  valley <- rlang::arg_match(valley)

  topo <- inject_coords(topo, x = x0)
  # Ensure sorted by x
  ord <- order(topo[, 1])
  topo <- topo[ord, , drop = FALSE]
  # Remove points to the left of x0
  topo <- topo[topo[, 1] >= x0, , drop = FALSE]
  n_topo <- nrow(topo)
  y <- topo[, 2] - thalweg_height
  x <- topo[, 1]

  # Stop early if v = 0.
  if (v == 0) {
    return(0)
  }

  # If there are no points to the right of x0 (e.g. x0 is at or beyond the
  # rightmost point of the topo), there is no volume available to satisfy
  # v > 0.
  if (n_topo < 2) {
    stop("Requested volume exceeds what is available to the right of x0")
  }

  # Inject crossing points where topo crosses thalweg height.
  # This allows us to account for part of a trapezoid dipping below
  # the thalweg height by splitting the trapezoid into two and leveling-out
  # the part below the thalweg height.
  crosses <- sign(y[-1]) * sign(y[-n_topo]) < 0
  x_cross <- x[-n_topo][crosses] +
    (0 - y[-n_topo][crosses]) *
      (x[-1][crosses] - x[-n_topo][crosses]) /
      (y[-1][crosses] - y[-n_topo][crosses])
  if (length(x_cross)) {
    topo <- inject_coords(topo, x = x_cross)
    topo <- topo[order(topo[, 1]), ]
    y <- topo[, 2] - thalweg_height
    x <- topo[, 1]
  }

  # Floor heights relative to thalweg
  y <- pmax(y, 0)

  # Trapezoid areas between consecutive x's
  widths <- diff(x)
  areas <- (y[-length(y)] + y[-1]) / 2 * widths

  # Cumulative sum of areas
  cum_areas <- cumsum(areas)

  # Check if target volume available
  if (v > utils::tail(cum_areas, 1)) {
    stop("Requested volume exceeds what is available to the right of x0")
  }

  # In the off chance that the cumulative area exactly matches v,
  # and comes up against a valley (below the thalweg), use left and right
  # valley input.
  seg_idx <- which(cum_areas == v)
  if (length(seg_idx) > 0) {
    if (valley == "left") {
      return(x[1 + seg_idx[1]] - x0)
    } else {
      return(x[1 + utils::tail(seg_idx, 1)] - x0)
    }
  }

  # Segment where cumulative passes v
  seg_idx <- which(cum_areas >= v)[1]
  if (seg_idx == 1) {
    cum_before <- 0
  } else {
    cum_before <- cum_areas[seg_idx - 1]
  }
  v_remain <- v - cum_before

  # Heights & width for this segment
  h1 <- y[seg_idx]
  h2 <- y[seg_idx + 1]
  w <- widths[seg_idx]

  # Solve for delta_x in trapezoid using quadratic equation
  # ax^2 + bx - v_remain = 0.
  a <- (h2 - h1) / (2 * w)
  b <- h1
  if (abs(a) < .Machine$double.eps) {
    # Heights equal (rectangle)
    delta_x <- v_remain / h1
  } else {
    delta_x <- (-b + sqrt(b^2 + 4 * a * v_remain)) / (2 * a)
  }

  delta_x
}
