#' Height of left bank above thalweg
#'
#' Calculate the heights of left banks above thalweg for all cross sections.
#'
#' @param channel A channel object with profile cross sections
#' @returns A vector of left bank heights, one for each cross section
#' @export
xt_height_lb <- function(channel) {
  checkmate::assert_class(channel, "sxchan")
  
  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }
  
  vapply(profile, function(xs) max(xs$left$bank_point[2] - xs$left$thalweg[2], 0), numeric(1))
}

#' Height of right bank above thalweg
#'
#' Calculate the heights of right banks above thalweg for all cross sections.
#'
#' @param channel A channel object with profile cross sections
#' @returns A vector of right bank heights, one for each cross section
#' @export
xt_height_rb <- function(channel) {
  checkmate::assert_class(channel, "sxchan")
  
  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }
  
  vapply(profile, function(xs) max(xs$right$bank_point[2] - xs$right$thalweg[2], 0), numeric(1))
}

#' Channel height using elevation specifications
#'
#' Calculate channel height as the difference between upper and lower elevation references.
#'
#' @param channel A channel object with profile cross sections
#' @param lower Lower elevation specification (defaults to thalweg)
#' @param upper Upper elevation specification (defaults to minimum bank elevation)
#' @returns A vector of heights, one for each cross section
#' @details This function calculates channel height as the difference between
#' upper and lower elevation references. The elevation specifications determine
#' which elevation values are used for the calculation.
#' @examples
#' # Get height from thalweg to minimum bank elevation
#' heights <- xt_height(channel)
#' 
#' # Get height from thalweg to water surface
#' heights <- xt_height(channel, upper = elevation_column("water_surface"))
#' 
#' # Get height from channel bottom to maximum bank elevation
#' heights <- xt_height(channel, 
#'                     lower = elevation_bottom(.f = mean),
#'                     upper = elevation_bank(.f = max))
#' @export
xt_height <- function(channel, lower = elevation_thalweg(), upper = elevation_bank()) {
  checkmate::assert_class(channel, "sxchan")
  
  # Get elevation values
  lower_elevations <- xt_elevation(channel, reference = lower)
  upper_elevations <- xt_elevation(channel, reference = upper)
  
  # Calculate height as difference
  upper_elevations - lower_elevations
}
