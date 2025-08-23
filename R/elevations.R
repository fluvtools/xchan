#' Create a thalweg elevation specification
#'
#' @returns An elevation specification that returns thalweg elevation
#' @export
elevation_thalweg <- function() {
  f <- function(channel) {
    profile <- xt_column_profile(channel)
    if (is.null(profile)) {
      stop("Channel object must have profile cross sections")
    }
    
    vapply(profile, function(xs) {
      # Get thalweg elevation (minimum elevation in the profile)
      left_coords <- xs$left$coordinates
      right_coords <- xs$right$coordinates
      all_coords <- rbind(left_coords, right_coords)
      min(all_coords[, 2])
    }, numeric(1))
  }
  structure(
    f,
    name = "Thalweg Elevation",
    params = list(),
    class = "sxchan_elevation"
  )
}

#' Create a column-based elevation specification
#'
#' @param column_name Name of the column containing elevation values
#' @returns An elevation specification that returns values from a channel column
#' @export
elevation_column <- function(column_name) {
  f <- function(channel) {
    if (!column_name %in% names(channel)) {
      stop("Column '", column_name, "' not found in channel object")
    }
    channel[[column_name]]
  }
  structure(
    f,
    name = "Column Elevation",
    params = list(column_name = column_name),
    class = "sxchan_elevation"
  )
}

#' Create a constant elevation specification
#'
#' @param value Constant elevation value
#' @returns An elevation specification that returns a constant value
#' @export
elevation_constant <- function(value) {
  f <- function(channel) {
    n <- nrow(channel)
    rep(value, n)
  }
  structure(
    f,
    name = "Constant Elevation",
    params = list(value = value),
    class = "sxchan_elevation"
  )
}

#' Create a bank elevation specification
#'
#' @param .f Function to apply to bank heights (e.g., min, max, mean)
#' @returns An elevation specification that returns aggregated bank elevations
#' @export
elevation_bank <- function(.f = min) {
  f <- function(channel) {
    profile <- xt_column_profile(channel)
    if (is.null(profile)) {
      stop("Channel object must have profile cross sections")
    }
    
    vapply(profile, function(xs) {
      left_bank <- xs$left$bank_point[2]
      right_bank <- xs$right$bank_point[2]
      .f(left_bank, right_bank)
    }, numeric(1))
  }
  structure(
    f,
    name = "Bank Elevation",
    params = list(.f = .f),
    class = "sxchan_elevation"
  )
}

#' Create a bottom elevation specification
#'
#' @param .f Function to apply to channel bottom elevations (e.g., min, max, mean, quantile)
#' @param ... Additional arguments for .f (e.g., probs for quantile)
#' @returns An elevation specification that returns aggregated bottom elevations
#' @export
elevation_bottom <- function(.f = mean, ...) {
  f <- function(channel) {
    profile <- xt_column_profile(channel)
    if (is.null(profile)) {
      stop("Channel object must have profile cross sections")
    }
    
    vapply(profile, function(xs) {
      # Get all coordinates within the channel (between banks)
      left_coords <- xs$left$coordinates
      right_coords <- xs$right$coordinates
      
      # Find points between banks
      left_bank_dist <- xs$left$bank_point[1]
      right_bank_dist <- xs$right$bank_point[1]
      
      # Filter coordinates within channel
      left_in_channel <- left_coords[, 1] >= left_bank_dist
      right_in_channel <- right_coords[, 1] <= right_bank_dist
      
      channel_coords <- rbind(
        left_coords[left_in_channel, , drop = FALSE],
        right_coords[right_in_channel, , drop = FALSE]
      )
      
      if (nrow(channel_coords) == 0) {
        return(NA)
      }
      
      # Apply function to elevations
      do.call(.f, c(list(channel_coords[, 2]), list(...)))
    }, numeric(1))
  }
  structure(
    f,
    name = "Bottom Elevation",
    params = list(.f = .f, ... = list(...)),
    class = "sxchan_elevation"
  )
}


