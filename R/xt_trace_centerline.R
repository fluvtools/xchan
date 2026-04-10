#' Trace centerline from channel
#'
#' Trace a centerline by connecting the midpoints of planimetric cross sections.
#'
#' @param channel Channel object with planimetric cross sections
#' @returns An sf LINESTRING representing the centerline
#' @details This function extracts the planimetric cross sections from the channel
#' object and connects their midpoints to create a centerline. The centerline
#' represents the approximate flow path through the channel.
#' @examples
#' centerline <- xt_trace_centerline(channel)
#' plot(centerline, col = "red", lwd = 2)
#' @export
xt_trace_centerline <- function(channel) {
  if (!is_channel(channel)) {
    stop("Input must be a channel object")
  }

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  # Extract midpoints of each cross section
  midpoints <- lapply(plan, function(xs) {
    coords <- sf::st_coordinates(xs)
    # Cross sections are lines; take the midpoint between their endpoints.
    endpoint_midpoint <- colMeans(rbind(coords[1, 1:2], coords[nrow(coords), 1:2]))
    sf::st_point(endpoint_midpoint)
  })

  # Convert to sf points
  midpoint_sfc <- sf::st_sfc(midpoints, crs = sf::st_crs(plan))

  # Create centerline by connecting midpoints
  centerline_coords <- sf::st_coordinates(midpoint_sfc)
  centerline <- sf::st_linestring(centerline_coords)

  sf::st_sfc(centerline, crs = sf::st_crs(plan))
}
