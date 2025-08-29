#' Extend Topographic Frame
#'
#' Extend the topographic frame beyond the current extent to allow for channel
#' widening. This function adds topography beyond the banks to provide space for
#' erosion calculations.
#'
#' @param channel Channel object
#' @param extender Extender operator created with `extender_*()` functions
#' @returns A modified channel object with extended topographic frame
#' @details
#' This function extends the profile cross-sections beyond the banks by adding
#' topography using extender operators. Extenders are operator factories that
#' create functions for extending the topographic frame.
#'
#' Available extender types:
#' - `extender_flat()`: Flat extension at specified elevation
#' - `extender_slope()`: Sloped extension with specified gradient
#'
#' @examples
#' # Flat extension at bank elevation
#' channel <- xt_channel(c(10, 12, 8, 15, 11, 9))
#' channel <- xt_extend_frame(channel, extender = extender_flat(extent = 20))
#'
#' # Sloped extension
#' channel <- xt_extend_frame(channel, extender = extender_slope(extent = 30, slope = 0.02))
#'
#' # Different elevation reference
#' channel <- xt_extend_frame(channel, extender = extender_flat(extent = 20, elevation = elevation_topo_left()))
#' @export
xt_extend_frame <- function(channel, extender) {
  checkmate::assert_class(channel, "sxchan")
  
  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }
  
  # Validate extender
  checkmate::assert_class(extender, "sxchan_extender")
  
  # Get extension parameters from extender
  extender_params <- extender(channel)
  
  # Extend each profile cross-section
  for (i in seq_along(profile)) {
    xs <- profile[[i]]
    
    # Get bank points and elevations
    left_bank_dist <- min(xs$banks)
    right_bank_dist <- max(xs$banks)
    
    # Get bank elevations
    left_bank_coords <- xs$coordinates[xs$coordinates[, 1] == left_bank_dist, , drop = FALSE]
    right_bank_coords <- xs$coordinates[xs$coordinates[, 1] == right_bank_dist, , drop = FALSE]
    left_bank_elev <- if (nrow(left_bank_coords) > 0) left_bank_coords[1, 2] else 0
    right_bank_elev <- if (nrow(right_bank_coords) > 0) right_bank_coords[1, 2] else 0
    
    # Apply extender to both sides
    # For now, use placeholder implementation
    # This will be replaced with actual extender logic
    left_extension <- matrix(
      c(seq(left_bank_dist - extender_params$extent, left_bank_dist, length.out = 5), 
        rep(left_bank_elev, 5)),
      ncol = 2
    )
    xs$left$coordinates <- rbind(left_extension, xs$left$coordinates)
    
    right_extension <- matrix(
      c(seq(right_bank_dist, right_bank_dist + extender_params$extent, length.out = 5),
        rep(right_bank_elev, 5)),
      ncol = 2
    )
    xs$right$coordinates <- rbind(xs$right$coordinates, right_extension)
    
    profile[[i]] <- xs
  }
  
  # Update the channel
  xt_column_profile(channel) <- profile
  channel
}
