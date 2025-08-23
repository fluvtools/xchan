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
  ymin <- xs2d$left$thalweg[2]
  xs2d$left$coordinates[, 2]  <- ymin + times * (xs2d$left$coordinates[, 2] - ymin)
  xs2d$left$bank_point[2]         <- ymin + times * (xs2d$left$bank_point[2] - ymin)
  xs2d$left$thalweg[2]      <- ymin + times * (xs2d$left$thalweg[2] - ymin)
  xs2d$right$coordinates[, 2] <- ymin + times * (xs2d$right$coordinates[, 2] - ymin)
  xs2d$right$bank_point[2]        <- ymin + times * (xs2d$right$bank_point[2] - ymin)
  xs2d$right$thalweg[2]     <- ymin + times * (xs2d$right$thalweg[2] - ymin)
  xs2d
}
