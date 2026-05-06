#' Calculate Downstream Distance of Cross Sections
#'
#' Calculates the distance each cross section in a channel is downstream from the head.
#'
#' @param channel A channel object with planimetric cross sections
#' @param axis Optional. An sf LINESTRING representing the channel axis (line
#' along the channel) for downstream ordering and distance. If `NULL` (default),
#' uses the centerline from `xt_trace_centerline()` with its default settings.
#' @returns A numeric vector of distances downstream (along the axis)
#' for each cross section in `channel`, where 0 represents the start of the
#' axis.
#' @note This is a useful function for sorting cross sections in a channel
#' if they are not already sorted.
#' @examples
#' # Using the auto-generated axis (midpoint trace)
#' distances <- xt_distance_ds(demo_channel)
#' @export
xt_distance_ds <- function(channel, axis = NULL) {
  if (!is_channel(channel)) {
    stop("Input must be a channel object")
  }

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  # Use provided axis or trace one from cross-section midpoints
  if (is.null(axis)) {
    axis <- xt_trace_centerline(channel)
  } else {
    if (
      !inherits(axis, "sfc") ||
        !all(sf::st_geometry_type(axis) == "LINESTRING")
    ) {
      stop("axis must be an sf LINESTRING")
    }
  }

  # Extract midpoints of each cross section
  midpoints <- lapply(plan, function(xs) {
    coords <- sf::st_coordinates(xs)
    midpoint_idx <- ceiling(nrow(coords) / 2)
    sf::st_point(coords[midpoint_idx, 1:2])
  })

  # Convert to sf points
  midpoint_sfc <- sf::st_sfc(midpoints, crs = sf::st_crs(plan))

  # Distances along the axis
  distances <- numeric(length(midpoints))

  for (i in seq_along(midpoints)) {
    # Closest point on the axis to each cross-section midpoint
    nearest_pt <- sf::st_nearest_points(midpoint_sfc[i], axis)
    nearest_pt_on_line <- sf::st_cast(nearest_pt, "POINT")[2] # The second point is on the line

    # Distance from the start of the axis to this point
    if (i == 1) {
      distances[i] <- 0
    } else {
      frac <- sf::st_line_project(
        axis,
        nearest_pt_on_line,
        normalized = TRUE
      )
      line_segment <- lwgeom::st_linesubstring(axis, 0, frac)
      distances[i] <- sf::st_length(line_segment)
    }
  }

  return(as.numeric(distances))
}
