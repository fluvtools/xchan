#' Create a rectangular dredger
#'
#' @param depth Depth to dredge below the water surface
#' @param wse Water surface elevation specification
#' @returns A dredger object that takes a channel and returns a modified channel
#' @export
dredger_rectangle <- function(depth, wse = elevation_bank()) {
  f <- function(channel) {
    if (!is_channel(channel)) {
      stop("Input must be a channel object")
    }
    
    profile <- xt_column_profile(channel)
    if (is.null(profile)) {
      stop("Channel object must have profile cross sections")
    }
    
    # Get water surface elevations using the elevation specification
    water_surface <- xt_elevation(channel, reference = wse)
    
    # Apply dredging to each profile
    for (i in seq_along(profile)) {
      profile[[i]] <- dredge_profile_rectangle(profile[[i]], depth, water_surface[i])
    }
    
    # Update the channel with new profiles
    xt_column_profile(channel) <- profile
    channel
  }
  structure(
    f,
    name = "Rectangle Dredger",
    params = list(depth = depth, wse = wse),
    class = "sxchan_dredger"
  )
}

#' Create a spline dredger
#'
#' @param alpha Smoothing parameter for spline fitting
#' @param beta Weight parameter for surrounding topography
#' @returns A dredger object that takes a channel and returns a modified channel
#' @export
dredger_spline <- function(alpha = 0.3, beta = 0.5) {
  structure(
    function(channel, water_surface = NULL) {
      if (!is_channel(channel)) {
        stop("Input must be a channel object")
      }
      
      profile <- xt_column_profile(channel)
      if (is.null(profile)) {
        stop("Channel object must have profile cross sections")
      }
      
      # If no water surface provided, use minimum bank height
      if (is.null(water_surface)) {
        water_surface <- xt_height(channel, .f = min)
      }
      
      # Placeholder for spline dredging algorithm
      # This would implement the smooth curve generation based on surrounding topography
      stop("Spline dredger not yet implemented")
    },
    name = "Spline Dredger",
    params = list(alpha = alpha, beta = beta),
    class = "sxchan_dredger"
  )
}



#' Helper function to dredge a single profile to rectangular shape
#'
#' @param profile xs_profile object
#' @param depth Depth to dredge
#' @param water_surface Water surface elevation
#' @returns Modified xs_profile object
dredge_profile_rectangle <- function(profile, depth, water_surface) {
  # Get bank points
  left_bank <- profile$left$bank_point
  right_bank <- profile$right$bank_point
  
  # Calculate dredge elevation
  dredge_elevation <- water_surface - depth
  
  # Find points between banks and modify their elevations
  left_coords <- profile$left$coordinates
  right_coords <- profile$right$coordinates
  
  # Modify left bank coordinates
  left_bank_dist <- left_bank[1]
  left_indices <- left_coords[, 1] >= left_bank_dist
  left_coords[left_indices, 2] <- pmax(left_coords[left_indices, 2], dredge_elevation)
  
  # Modify right bank coordinates
  right_bank_dist <- right_bank[1]
  right_indices <- right_coords[, 1] <= right_bank_dist
  right_coords[right_indices, 2] <- pmax(right_coords[right_indices, 2], dredge_elevation)
  
  # Update profile
  profile$left$coordinates <- left_coords
  profile$right$coordinates <- right_coords
  
  profile
}

#' Create a DEM dredger for channel modification
#'
#' @param dem DEM raster containing bathymetry data
#' @param sample_freq Sampling frequency along cross-sections
#' @param sample_n Number of sample points per cross-section
#' @returns A dredger object that takes a channel and returns a modified channel
#' @details
#' Must specify only one of `sample_freq` or `sample_n`.
#' This dredger is particularly useful when you have a separate bathymetry DEM
#' in addition to a topography DEM that doesn't include bathymetry. The bathymetry
#' DEM can be used to carve out the channel bottom while preserving the surrounding
#' topography from the main DEM.
#' @export
dredger_dem <- function(dem, ..., sample_freq, sample_n) {
  checkmate::assert_class(dem, "SpatRaster")
  structure(
    function(channel, water_surface = NULL) {
      if (!is_channel(channel)) {
        stop("Input must be a channel object")
      }
      
      profile <- xt_column_profile(channel)
      if (is.null(profile)) {
        stop("Channel object must have profile cross sections")
      }
      
      # Placeholder for DEM dredging algorithm
      # This would sample the bathymetry DEM along each cross-section
      # and modify the profile accordingly
      stop("DEM dredger not yet implemented")
    },
    name = "DEM Dredger",
    params = list(dem = dem, sample_freq = sample_freq, sample_n = sample_n),
    class = "sxchan_dredger"
  )
}
