erode_right_xs <- function(XsecLine, pt_n, dem, extent, width){

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
  all_points_sf <- sf::st_as_sf(data.frame(id = 1:length(all_points)), geometry = sf::st_sfc(all_points, crs = sf::st_crs(XsecLine)))
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

  # Save the original cross-section for comparison
  original_cross_section <- cross_section

  xs_matrix <- as.matrix(cross_section[, c("relative_distance", "elevation")])

  # Determine left and right bank points
  original_left <- min(cross_section$relative_distance) + extent / 2
  original_right <- max(cross_section$relative_distance) - extent / 2

  xs <- xt_cross_section(xs_matrix, original_left, original_right)



  bank <- data.frame(
    relative_distance = xs$right$bank[1],
    elevation = xs$right$bank[2])


  # Identify the thalweg (lowest elevation point)
  thalweg <- data.frame(
    relative_distance = xs$right$thalweg[1],
    elevation = xs$right$thalweg[2]
  )


  # Add the right bank point to the cross_section
  cross_section <- rbind(
    cross_section[c(5,3)],
    bank
  )

  # Sort cross_section by relative distance
  cross_section <- cross_section[order(cross_section$relative_distance), ]

  # Add shifted thalweg point
  shifted_thalweg <- data.frame(
    relative_distance = thalweg$relative_distance + width,
    elevation = thalweg$elevation
  )

  # Identify points to shift (thalweg to right bank)
  to_shift <- which(cross_section$relative_distance >= thalweg$relative_distance &
                      cross_section$relative_distance <= bank$relative_distance + width)

  # Shift selected points
  shifted_points <- cross_section[to_shift, ]
  shifted_points$relative_distance <- shifted_points$relative_distance + width

  # Calculate vertical line points
  bottom_vertical_point <- data.frame(
    relative_distance = bank$relative_distance + width,
    elevation = bank$elevation)

  original_cross_section <- original_cross_section[!duplicated(original_cross_section), ]

  top_vertical_point <- data.frame(
    relative_distance = bank$relative_distance + width,
    elevation = approx(
      x = original_cross_section$relative_distance,
      y = original_cross_section$elevation,
      xout = bank$relative_distance + width,
      rule = 2
    )$y
  )

  #if top point is lower than the bottom point, this means that the parallel side due to extenion went outside the cross section
  #to solve this we should remove adding the cliff and only include the interpolated point between the original xsec with the extended width
  # (this is the top point)
  if (top_vertical_point$elevation < bottom_vertical_point$elevation){
    bottom_vertical_point$elevation <- NA
  }


  #remove any point higher than the bottom elevation of the vertical line
  # if (mean(shifted_points$elevation) > bank$elevation){
  #   shifted_points <- shifted_points
  # } else {
  #   shifted_points <- shifted_points[shifted_points$elevation < bottom_vertical_point[1,2], ]
  # }

  # Remove the original **right** bank point and points between thalweg and new left bank
  cross_section1 <- original_cross_section[(original_cross_section$relative_distance >= top_vertical_point$relative_distance), ]
  cross_section2 <- original_cross_section[(original_cross_section$relative_distance <= thalweg$relative_distance), ]

  # Create the modified cross-section
  modified_cross_section <- rbind(cross_section2[c(5,3)],shifted_points,shifted_thalweg, thalweg, bottom_vertical_point,
                                  top_vertical_point, cross_section1[c(5,3)])
  modified_cross_section <- na.omit(modified_cross_section)
  # Remove duplicates
  modified_cross_section <- modified_cross_section[!duplicated(modified_cross_section), ]

  xs_matrix <- as.matrix(modified_cross_section[, c("relative_distance", "elevation")])
  xs_matrix <- na.omit(xs_matrix)

  # Determine left and right bank points
  original_left <- xs[["left"]][["bank"]][1]
  original_right <- top_vertical_point$relative_distance[1]

  xs_obj <- xt_cross_section(xs_matrix, original_left, original_right)

  return(xs_obj)
}
