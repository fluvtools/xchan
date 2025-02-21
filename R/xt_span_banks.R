#' Generate a line segment from bank to bank
#'
#' Given a point within a channel, generate a line segment that goes from
#' bank to bank, for a specified angle.
#' @param pt A point within the channel.
#' @param bankline The bankline of the channel.
#' @param angle The angle of the line segment, in radians.
#' @return A line segment spanning from bank to bank.
#' @note This function is the precursor to generating cross sections with
#' `xt_generate_xsc()`.
#' @examples
#' # Pick a point in the channel, and an angle
#' library(sf)
#' pt <- st_centroid(demo_bankline) - c(0, 30)
#' plot(demo_bankline)
#' plot(pt, add = TRUE)
#'
#' ## 45 degrees:
#' span <- xt_span_banks(pt, angle = pi / 4, bankline = demo_bankline)
#' plot(span, add = TRUE, col = "blue")
#'
#' ## 0 degrees:
#' span <- xt_span_banks(pt, angle = 0, bankline = demo_bankline)
#' plot(span, add = TRUE, col = "blue")
#' @export
xt_span_banks <- function(pt, angle, bankline) {
  bb <- sf::st_bbox(bankline)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )
  span_banks_engine(
    pt, angle, bankline = bankline, maxd = maxd, intersect = TRUE,
    reposition = TRUE
  )
}

span_banks_engine <- function(
    pt, angle, bankline, maxd, intersect, reposition
) {
  if (!identical(bankline, sf::st_geometry(bankline))) {
    stop("bankline input must be a geometry.")
  }
  pt_coord <- sf::st_coordinates(pt)
  # Move the whole channel so that first_pt is at the origin
  bl_moved <- bankline - pt_coord
  # Construct a horizonal line going through the origin; but make sure
  # it's made up of three points -- the two ends, and the origin itself,
  # so that when it's rotated, (0, 0) is always on the line (otherwise,
  # it wouldn't be, as a result of rounding errors)
  horizontal_line <- sf::st_geometry(
    sf::st_linestring(rbind(
      c(-maxd, 0),
      #c(0, 0),
      c(maxd, 0)
    ))
  )
  # Construct the rotation matrix for a given angle (deliberately the transpose
  # of the usual rotation matrix because the matrix needs to be on the RHS of
  # the line to be rotated)
  cos_angle <- cos(angle)
  sin_angle <- sin(angle)
  rotation_matrix <- matrix(
    c(cos_angle, -sin_angle, sin_angle, cos_angle),
    ncol = 2
  )
  # Rotate the horizontal line by the given angle
  angled_line <- horizontal_line * rotation_matrix
  if (!intersect) {
    if (reposition) {
      return(angled_line + pt_coord)
    } else {
      return(angled_line)
    }
  }
  # Find the intersection of the rotated line with the banklines, and split
  # out each intersection segment as a separate feature.
  intersections <- sf::st_intersection(angled_line, bl_moved)
  intersections <- sf::st_cast(intersections, "LINESTRING")
  # Only take the intersection segment that goes through the origin.
  # foo <- sf::st_intersects(intersections, sf::st_point(c(0, 0)), sparse = FALSE)
  # relevant_segment <- intersections[foo]
  distances <- sf::st_distance(intersections, sf::st_point(c(0, 0)))[, 1]
  relevant_segment <- intersections[distances == min(distances)]
  if (reposition) {
    return(relevant_segment + pt_coord)
  } else {
    return(relevant_segment)
  }
}
