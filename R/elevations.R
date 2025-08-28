#' Elevation Specifications
#'
#' Elevation specifications define how to calculate elevation values from
#' channel cross-sections. These functions return elevation specification
#' objects that can be used in various channel operations.
#'
#' @param .f Function to apply to elevation values (e.g., mean, min, max)
#' @param ... Additional arguments passed to `.f`
#' @returns An elevation specification object
#' @rdname elevations
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
  structure(f, name = "thalweg", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_bank <- function(.f = min) {
  f <- function(channel) {
    # Implementation will extract bank elevations and apply .f
    # For now, return a placeholder
    rep(0, xt_n_sections(channel))
  }
  structure(f, name = "bank", params = list(.f = .f), class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_bank_left <- function() {
  f <- function(channel) {
    # Implementation will extract left bank elevations and apply .f
    # For now, return a placeholder
    rep(0, xt_n_sections(channel))
  }
  structure(f, name = "bank_left", params = list(.f = .f), class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_bank_right <- function() {
  f <- function(channel) {
    # Implementation will extract right bank elevations and apply .f
    # For now, return a placeholder
    rep(0, xt_n_sections(channel))
  }
  structure(f, name = "bank_right", params = list(.f = .f), class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_topo_left <- function() {
  f <- function(channel) {
    # Implementation will extract leftmost point elevations
    # For now, return a placeholder
    rep(0, xt_n_sections(channel))
  }
  structure(f, name = "topo_left", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_topo_right <- function() {
  f <- function(channel) {
    # Implementation will extract rightmost point elevations
    # For now, return a placeholder
    rep(0, xt_n_sections(channel))
  }
  structure(f, name = "topo_right", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_topo <- function(.f = mean, ...) {
  f <- function(channel) {
    # Implementation will extract all topography elevations and apply .f
    # For now, return a placeholder
    rep(0, xt_n_sections(channel))
  }
  structure(f, name = "topo", params = list(.f = .f, ...), class = "sxchan_elevation")
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


