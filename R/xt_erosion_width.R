#' Calculate Erosion Width from Volume Change
#'
#' This function calculates the erosion width for each cross-section in a channel
#' given a specified volume change, distributing the change according to a given scheme.
#'
#' @param channel Channel object
#' @param volume Volume of erosion; single positive numeric or vector matching number of cross-sections.
#' @param side A specification for how to distribute the widening between
#' left and right banks. Built-in splitters include "left", "right", and "both".
#' @param error_on_overflow Logical; should an error be thrown if asked
#' to calculate erosion width beyond cross section extent? `TRUE` if so
#' (the default). If `FALSE`, returns the maximum width up to the extent.
#' @returns A numeric vector of erosion widths for each cross-section in the channel.
#' @examples
#' xt_erosion_width(channel, volume = 50, side = "left")
#' xt_erosion_width(channel, volume = 50, side = splitter_left(0.75))
#' @export
xt_erosion_width <- function(channel, volume, side = "both", error_on_overflow = TRUE) {
  checkmate::assert_class(channel, "sxchan")
  
  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }
  
  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)
  
  # Recycle volume to match number of cross-sections
  volume <- vctrs::vec_recycle(volume, length(profile))
  
  # Calculate erosion width for each cross-section
  widths <- numeric(length(profile))
  censored <- logical(length(profile))
  
  for (i in seq_along(profile)) {
    xs <- profile[[i]]
    dv_left <- volume[i] * prop_left[i]
    dv_right <- volume[i] - dv_left
    
    dw1 <- xt_erosion_width_left(xs, dv_left, error_on_overflow = error_on_overflow)
    xs_flipped <- flip_xs2d(xs)
    dw2 <- xt_erosion_width_left(xs_flipped, dv_right, error_on_overflow = error_on_overflow)
    
    widths[i] <- dw1 + dw2
    censored[i] <- attr(dw1, "censored") || attr(dw2, "censored")
  }
  
  attr(widths, "censored") <- censored
  widths
}

xt_erosion_width_left <- function(xs, volume, error_on_overflow = TRUE) {
  checkmate::assert_numeric(volume, 0, len = 1, any.missing = FALSE)
  if (volume == 0) {
    w <- 0
    attr(w, "censored") <- FALSE
    return(w)
  }
  x_old <- xs$left$bank_point[1]
  y_bank <- xs$left$bank_point[2]
  left_nodes <- xs$left$coordinates
  x_extent <- min(left_nodes[, 1])
  
  # Use find_x_for_volume_right to find the new x position
  x_new <- find_x_for_volume_right(
    v = volume,
    x0 = x_old,
    topo = left_nodes,
    thalweg_height = y_bank,
    valley = "left"
  )
  
  censored <- FALSE
  if (x_new < x_extent) {
    if (error_on_overflow) {
      stop(
        "Cannot calculate erosion width for given change in volume, as ",
        "the cross section extent is surpassed."
      )
    } else {
      # Calculate maximum possible width
      x_new <- x_extent
      censored <- TRUE
    }
  }
  
  w <- x_old - x_new
  attr(w, "censored") <- censored
  w
}
