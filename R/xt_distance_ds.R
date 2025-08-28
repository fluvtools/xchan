#' Calculate Downstream Distance of Cross Sections
#'
#' Calculates the distance each cross section in a channel is downstream from the head.
#'
#' @param channel A channel object with planimetric cross sections
#' @param flowline Optional. An sf LINESTRING representing the line of flow,
#' such as the centerline. If `NULL` (default), uses the centerline
#' generated using `xt_trace_centerline(channel)` with its default settings.
#' @returns A numeric vector of distances downstream (along the line of flow)
#' for each cross section in `channel`, where 0 represents the start of the
#' line of flow.
#' @note This is a useful function for sorting cross sections in a channel
#' if they are not already sorted.
#' @examples
#' # Using the auto-generated flowline
#' distances <- xt_distance_ds(demo_channel)
#' @export
xt_distance_ds <- function(channel, flowline = NULL) {
  if (!is_channel(channel)) {
    stop("Input must be a channel object")
  }

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  # Use provided flowline or generate one if not provided
  if (is.null(flowline)) {
    flowline <- xt_centerline(channel)
  } else {
    # Validate that flowline is an sf LINESTRING
    if (!inherits(flowline, "sfc") || !all(sf::st_geometry_type(flowline) == "LINESTRING")) {
      stop("Flowline must be an sf LINESTRING")
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

  # Calculate the distances along the flowline
  distances <- numeric(length(midpoints))

  for (i in seq_along(midpoints)) {
    # Find the closest point on the flowline to each midpoint
    nearest_pt <- sf::st_nearest_points(midpoint_sfc[i], flowline)
    nearest_pt_on_line <- sf::st_cast(nearest_pt, "POINT")[2] # The second point is on the line

    # Calculate the distance from the start of the flowline to this point
    if (i == 1) {
      # For the first cross-section, distance is 0
      distances[i] <- 0
    } else {
      # For subsequent cross-sections, calculate distance along flowline
      line_segment <- sf::st_linesubstring(flowline, 0, sf::st_line_locate_point(flowline, nearest_pt_on_line))
      distances[i] <- sf::st_length(line_segment)
    }
  }

  return(as.numeric(distances))
}
