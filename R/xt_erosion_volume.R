#' Calculate Erosion Volume from Width Change
#'
#' This function calculates the erosion volume for each cross-section in a channel
#' given a specified width change, distributing the change according to a given scheme.
#'
#' @param channel Channel object
#' @param width Change in width; single positive numeric or vector matching number of cross-sections.
#' @param side A specification for how to distribute the widening between
#' left and right banks. Built-in splitters include "left", "right", and "both".
#' @param error_on_overflow Logical; should an error be thrown if asked
#' to calculate erosion volume beyond cross section extent? `TRUE` if so
#' (the default). If `FALSE`, returns the maximum volume up to the extent.
#' @returns A numeric vector of erosion volumes for each cross-section in the channel.
#' @examples
#' xt_erosion_volume(channel, width = 10, side = "left")
#' xt_erosion_volume(channel, width = 10, side = splitter_left(0.75))
#' @export
xt_erosion_volume <- function(channel, width, side = "both", error_on_overflow = TRUE) {
  checkmate::assert_class(channel, "sxchan")
  
  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }
  
  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)
  
  # Recycle width to match number of cross-sections
  width <- vctrs::vec_recycle(width, length(profile))
  
  # Calculate erosion volume for each cross-section
  volumes <- numeric(length(profile))
  censored <- logical(length(profile))
  
  for (i in seq_along(profile)) {
    xs <- profile[[i]]
    dw_left <- width[i] * prop_left[i]
    dw_right <- width[i] - dw_left
    
    v1 <- xt_erosion_volume_left(xs, dw_left, error_on_overflow = error_on_overflow)
    xs_flipped <- flip_xs2d(xs)
    v2 <- xt_erosion_volume_left(xs_flipped, dw_right, error_on_overflow = error_on_overflow)
    
    volumes[i] <- v1 + v2
    censored[i] <- attr(v1, "censored") || attr(v2, "censored")
  }
  
  attr(volumes, "censored") <- censored
  volumes
}

xt_erosion_volume_left <- function(xs, width, error_on_overflow = TRUE) {
  checkmate::assert_numeric(width, 0, len = 1, any.missing = FALSE)
  if (width == 0) {
    a <- 0
    attr(a, "censored") <- FALSE
    return(a)
  }
  x_old <- xs$left$bank_point[1]
  x_new <- x_old - width
  left_nodes <- xs$left$coordinates
  x_extent <- min(left_nodes[, 1])
  censored <- FALSE
  if (x_new < x_extent) {
    if (error_on_overflow) {
      stop(
        "Cannot calculate erosion volume for given change in width, as ",
        "the cross section extent is surpassed."
      )
    } else {
      width <- x_old - x_extent
      x_new <- x_extent
      censored <- TRUE
    }
  }
  y_thalweg <- xs$left$thalweg[2]
  left_nodes <- inject_2d_points(left_nodes, x_new)
  x_in_between <- left_nodes[, 1] >= x_new & left_nodes[, 1] <= x_old
  between_nodes <- left_nodes[x_in_between, , drop = FALSE]
  n <- nrow(between_nodes)
  nm1 <- n - 1
  delta_x <- between_nodes[2:n, 1] - between_nodes[1:nm1, 1]
  delta_y <- abs(between_nodes[2:n, 2] - between_nodes[1:nm1, 2])
  avg_y <- (between_nodes[2:n, 2] + between_nodes[1:nm1, 2]) / 2
  y_upper <- pmax(between_nodes[2:n, 2], between_nodes[1:nm1, 2])
  below_thalweg <- between_nodes[, 2] < y_thalweg
  zero_area <- below_thalweg[1:nm1] + below_thalweg[2:n] == 2
  partially_above <- below_thalweg[1:nm1] + below_thalweg[2:n] == 1
  area <- ifelse(
    partially_above,
    delta_x * (y_upper - y_thalweg)^2 / delta_y / 2,
    delta_x * (avg_y - y_thalweg)
  )
  area[zero_area] <- 0
  a <- sum(area)
  attr(a, "censored") <- censored
  a
}
