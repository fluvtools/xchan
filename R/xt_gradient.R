#' Calculate channel gradient
#'
#' @param channel Channel object
#' @param ... Additional arguments (ignored)
#' @param .before Number of cross-sections before current to include in calculation
#' @param .after Number of cross-sections after current to include in calculation
#' @param .complete Whether to include incomplete windows at boundaries
#' @param elevation Elevation specification for gradient calculation
#' @param centerline Centerline for distance calculations (defaults to generated centerline)
#' @returns A vector of gradient values for each cross-section
#' @details
#' This function calculates channel gradient using a sliding window approach,
#' inspired by the `slider` package. The gradient is calculated as the change
#' in elevation divided by the change in distance along the centerline.
#'
#' The elevation specification determines which elevation value is used for
#' each cross-section. The centerline is used to calculate distances between
#' cross-sections for the gradient calculation.
#' @examples
#' # Calculate gradient using thalweg elevation
#' gradient <- xt_gradient(channel, elevation = elevation_thalweg())
#'
#' # Calculate gradient using water surface elevation
#' gradient <- xt_gradient(channel, elevation = elevation_column("water_surface"))
#'
#' # Use wider window for smoother gradient
#' gradient <- xt_gradient(channel, .before = 2L, .after = 2L, elevation = elevation_bank(.f = mean))
#' @export
xt_gradient <- function(channel,
                        ...,
                        .before = 1L,
                        .after = 1L,
                        .complete = FALSE,
                        elevation = elevation_bank(),
                        centerline = NULL) {
  ellipsis::check_dots_empty()
  checkmate::assert_class(channel, "xchan")

  # Get elevation values for each cross-section
  elevations <- xt_elevation(channel, reference = elevation)

  # Generate centerline if not provided
  if (is.null(centerline)) {
    centerline <- xt_trace_centerline(channel)
  }

  # Calculate distances along centerline
  distances <- xt_distance_ds(channel, flowline = centerline)

  # Calculate gradient using sliding window
  n <- length(elevations)
  gradient <- numeric(n)

  for (i in seq_len(n)) {
    # Define window indices
    start_idx <- max(1, i - .before)
    end_idx <- min(n, i + .after)

    # Skip if window is incomplete and .complete = FALSE
    if (!.complete && (start_idx == 1 || end_idx == n)) {
      gradient[i] <- NA
      next
    }

    # Calculate gradient as change in elevation / change in distance
    if (end_idx > start_idx) {
      delta_elevation <- elevations[end_idx] - elevations[start_idx]
      delta_distance <- distances[end_idx] - distances[start_idx]
      gradient[i] <- delta_elevation / delta_distance
    } else {
      gradient[i] <- NA
    }
  }

  gradient
}
