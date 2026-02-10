#' Exaggerate Relief of 2D Cross Section
#'
#' Sometimes it's hard to see vertical relief in a plot of a 2D cross
#' section. This function exaggerates the relief by stretching it
#' by a multiplicative factor, used in the plotting functions.
#'
#' @param xs2d A single 2D cross section object.
#' @param times Multiplier to exaggerate the relief by. Single positive
#' numeric. Numbers >1 will stretch the relief; <1 will compress.
#' @returns The original cross section, with exaggerated elevations
#' (according to height above thalweg).
exaggerate_relief <- function(xs2d, times = 1) {
  checkmate::assert_numeric(times, 0, len = 1)
  
  # Get thalweg elevation (minimum elevation)
  ymin <- min(xs2d$coordinates[, 2])
  
  # Exaggerate all coordinates
  xs2d$coordinates[, 2] <- ymin + times * (xs2d$coordinates[, 2] - ymin)
  
  xs2d
}
