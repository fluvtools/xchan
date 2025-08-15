#' @export
plot.sxc2d <- function(x, ..., add = FALSE, exaggerate = 1) {
  if (!add) {
    plot(sf::st_sfc(
      sf::st_multilinestring(list(x$left$multiline)),
      sf::st_multilinestring(list(x$right$multiline))
    ))
  }
  plot(sf::st_sfc(sf::st_multilinestring(list(x$left$multiline))), add = TRUE, col = "blue")
  plot(sf::st_sfc(sf::st_multilinestring(list(x$right$multiline))), add = TRUE, col = "red")
  plot(sf::st_linestring(rbind(x$left$thalweg, x$right$thalweg)), add = TRUE)
  plot(sf::st_point(x$left$bank), add = TRUE, col = "blue")
  plot(sf::st_point(x$right$bank), add = TRUE, col = "red")
}

plot_2dxs <- function(XsecLine, dem, pt_n, extent, exaggerate) {
  # Compute total cross-section length
  extended_length <- as.numeric(sf::st_length(XsecLine))

  # Extract start and end points manually
  start_point <- sf::st_cast(sf::st_geometry(XsecLine), "POINT")[1]
  end_point <- sf::st_cast(sf::st_geometry(XsecLine), "POINT")[length(sf::st_geometry(XsecLine))]

  # Generate sampled points along the line
  sampled_points <- sf::st_line_sample(XsecLine, n = pt_n)

  # Combine start, sampled, and end points
  all_points <- c(
    sf::st_geometry(start_point),
    sf::st_geometry(sf::st_cast(sampled_points, "POINT")),
    sf::st_geometry(end_point)
  )

  # Convert all points into a valid sf object
  all_points_sf <- sf::st_as_sf(
    data.frame(id = 1:length(all_points)),
    geometry = sf::st_sfc(all_points, crs = sf::st_crs(XsecLine))
  )
  coords_matrix <- as.matrix(sf::st_coordinates(all_points_sf)[, 1:2])

  # Extract elevation values from the DEM
  elevations <- terra::extract(dem, coords_matrix)[, 1]

  # Create dataframe
  cross_section <- data.frame(
    x = coords_matrix[, 1],
    y = coords_matrix[, 2],
    elevation = elevations
  )

  # Compute segment-wise distances
  line_segments <- lapply(1:(nrow(cross_section) - 1), function(j) {
    sf::st_linestring(rbind(coords_matrix[j, ], coords_matrix[j + 1, ]))
  })

  # Convert to sf object
  line_segments_sf <- sf::st_sfc(line_segments, crs = sf::st_crs(XsecLine))
  segment_lengths <- as.numeric(sf::st_length(line_segments_sf))

  # Compute cumulative distances
  cumulative_distances <- c(0, cumsum(segment_lengths))
  cumulative_distances[length(cumulative_distances)] <- extended_length
  cross_section$distance <- cumulative_distances

  # Ensure cumulative distances match total length
  if (abs(max(cumulative_distances) - extended_length) > 0.001) {
    stop("Error: Final cumulative distance does not match expected total cross-section length.")
  }

  # Find centerline (midpoint of distance)
  centerline_idx <- which.min(abs(cross_section$distance - mean(cross_section$distance)))
  cross_section$relative_distance <- cross_section$distance - cross_section$distance[centerline_idx]


  xs_matrix <- as.matrix(cross_section[, c("relative_distance", "elevation")])

  # Determine left and right bank points
  original_left <- min(cross_section$relative_distance) + extent / 2
  original_right <- max(cross_section$relative_distance) - extent / 2

  xs <- xt_cross_section(xs_matrix, original_left, original_right)



  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = cross_section,
      mapping = ggplot2::aes(x = relative_distance, y = elevation),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = data.frame(
        relative_distance = xs$right$bank[1],
        elevation = xs$right$bank[2]
      ),
      ggplot2::aes(x = relative_distance, y = elevation),
      color = "blue", size = 3
    ) +
    ggplot2::geom_point(
      data = data.frame(relative_distance = xs$left$bank[1], elevation = xs$left$bank[2]),
      ggplot2::aes(x = relative_distance, y = elevation), color = "blue", size = 3
    ) +
    ggplot2::xlab("Relative Distance (m)") +
    ggplot2::ylab("Elevation (m)") +
    ggplot2::theme_minimal()
}
