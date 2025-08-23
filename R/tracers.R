#' Create a linear tracer for bankline generation
#'
#' @returns A tracer object that takes a channel and returns banklines
#' @export
tracer_linear <- function() {
  f <- function(channel) {
    plan <- xt_column_plan(channel)
    
    # Extract bank points from planimetric cross sections
    bank_points <- lapply(plan, function(xs) {
      coords <- sf::st_coordinates(xs)
      # Get leftmost and rightmost points
      left_bank <- coords[which.min(coords[, 1]), ]
      right_bank <- coords[which.max(coords[, 1]), ]
      rbind(left_bank, right_bank)
    })
    
    # Separate left and right bank points
    left_banks <- do.call(rbind, lapply(bank_points, function(x) x[1, ]))
    right_banks <- do.call(rbind, lapply(bank_points, function(x) x[2, ]))
    
    # Create polygon by connecting left banks, then right banks in reverse
    polygon_coords <- rbind(
      left_banks,
      right_banks[nrow(right_banks):1, ]
    )
    
    # Close the polygon
    polygon_coords <- rbind(polygon_coords, polygon_coords[1, ])
    
    sf::st_polygon(list(polygon_coords))
  }
  structure(
    f,
    name = "Linear Tracer",
    params = list(),
    class = "sxchan_tracer"
  )
}

#' Create a spline tracer for bankline generation
#'
#' @param degree Polynomial degree for spline fitting
#' @param smoothing Smoothing parameter for spline
#' @returns A tracer object that takes a channel and returns banklines
#' @export
tracer_spline <- function(degree = 3, smoothing = 0.5) {
  f <- function(channel) {
      plan <- xt_column_plan(channel)
      
      # Extract bank points from planimetric cross sections
      bank_points <- lapply(plan, function(xs) {
        coords <- sf::st_coordinates(xs)
        # Get leftmost and rightmost points
        left_bank <- coords[which.min(coords[, 1]), ]
        right_bank <- coords[which.max(coords[, 1]), ]
        rbind(left_bank, right_bank)
      })
      
      # Separate left and right bank points
      left_banks <- do.call(rbind, lapply(bank_points, function(x) x[1, ]))
      right_banks <- do.call(rbind, lapply(bank_points, function(x) x[2, ]))
      
      # Placeholder for Jane's spline algorithm
      # This would implement the specific spline algorithm Jane has written
      stop("Spline algorithm not yet implemented")
  }
  structure(
    f,
    name = "Spline Tracer",
    params = list(degree = degree, smoothing = smoothing),
    class = "sxchan_tracer"
  )
}

#' Create a DEM tracer for bankline generation
#'
#' @param dem DEM raster for contour tracing
#' @param contour_interval Interval between contours
#' @returns A tracer object that takes a channel and returns banklines
#' @export
tracer_dem <- function(dem, contour_interval = 1) {
  checkmate::assert_class(dem, "SpatRaster")
  f <- function(channel) {
      # Placeholder for DEM contour tracing algorithm
      stop("DEM tracer not yet implemented")
  }
  structure(
    f,
    name = "DEM Tracer",
    params = list(dem = dem, contour_interval = contour_interval),
    class = "sxchan_tracer"
  )
}


