#' Active Cross Section Widths
#'
#' Calculate the "active" width of channel cross sections, where an "active" width
#' is the width of the cross section occupied by water, not land. This is relevant when
#' there are islands / bars within the river.
#'
#' @param channel Channel object
#' @returns A numeric vector of length equal to the number of cross-sections in the channel
#' containing the active widths of each cross section.
#' @export
xt_width_active <- function(channel) {
  checkmate::assert_class(channel, "xchan")

  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }

  widths <- numeric(length(profile))

  for (i in seq_along(profile)) {
    xs <- profile[[i]]

    # Get all coordinates and sort by distance
    all_coords <- xs$coordinates[order(xs$coordinates[, 1]), , drop = FALSE]

    # Get bank distances from the new structure
    left_bank_dist <- min(xs$banks)
    right_bank_dist <- max(xs$banks)

    # Get bank elevations
    left_bank_coords <- xs$coordinates[
      xs$coordinates[, 1] == left_bank_dist,
      ,
      drop = FALSE
    ]
    right_bank_coords <- xs$coordinates[
      xs$coordinates[, 1] == right_bank_dist,
      ,
      drop = FALSE
    ]
    left_bank_elev <- if (nrow(left_bank_coords) > 0) {
      left_bank_coords[1, 2]
    } else {
      0
    }
    right_bank_elev <- if (nrow(right_bank_coords) > 0) {
      right_bank_coords[1, 2]
    } else {
      0
    }

    # Calculate active width (water width)
    widths[i] <- right_bank_dist - left_bank_dist
  }

  widths
}

# #' Calculate XS width when there are islands or not
# #'
# #' Workhorse for the `xs_width_multi()` function.
# #' Handles multiple cross sections; no need to run the function one at
# #' a time.
# #'
# #' @param coords The second entry from the output of `get_islandxs()`;
# #' A list of bank coordinates for each cross section.
# #' @param XsecLine Planimetric (1D) cross sections; object of class `sxc`.
# #' @returns A vector of widths, of length equal to the common lengths
# #' of the inputs, `coords` and `XsecLine`.
# xt_width_active <- function(coords, XsecLine) {
#
#   # Ensure the line has a CRS
#   if (is.null(sf::st_crs(XsecLine))) {
#     sf::st_crs(XsecLine) <- 3157  # Assign CRS if missing
#   }
#
#   # Ensure sampled_points has the same CRS
#   bank_points <- sf::st_multipoint(as.matrix(coords[, c("X", "Y")]))
#   bank_points <- sf::st_sfc(bank_points, crs = 3157)  # Convert to `sfc` with CRS
#
#   # Convert all points into a valid sf object
#   bank_points_sf <- sf::st_as_sf(
#     data.frame(id = 1:length(bank_points)),
#     geometry = sf::st_sfc(bank_points, crs = sf::st_crs(XsecLine))
#   )
#   coords_matrix <- as.matrix(sf::st_coordinates(bank_points_sf)[, 1:2])
#   # Extract elevation values from the DEM at these coordinates
#   # Create dataframe
#   cross_section <- data.frame(
#     x = coords_matrix[, 1],
#     y = coords_matrix[, 2]
#   )
#   cross_section <- cross_section |>
#     dplyr::mutate(
#       distance = sqrt((x - dplyr::lag(x))^2 + (y - dplyr::lag(y))^2)
#     )
#
#   # Compute the Euclidean distance between consecutive points
#   distances <- as.numeric(unlist(na.omit(cross_section$distance)))
#   # Get number of distances
#   n <- length(distances)
#   # Initialize labels
#   if (n == 1) {
#     names(distances) <- c("Channel Width")
#   } else if (n == 2) {
#     names(distances) <- c("Left Channel Width", "Right Channel Width")
#   } else {
#     labels <- c("Left Channel Width")  # First is always left channel width
#     # Assign alternating "Inner Channel Width" and "Inner Island Width"
#     for (i in 2:(n-1)) {
#       if (i %% 2 == 0) {
#         labels <- c(labels, "Inner Island Width")
#       } else {
#         labels <- c(labels, "Inner Channel Width")
#       }
#     }
#     labels <- c(labels, "Right Channel Width")  # Last is always right channel width
#     names(distances) <- labels
#   }
#
#   # Compute channel widths
#   channel_widths <- as.list(distances)
#   return(channel_widths)
# }
