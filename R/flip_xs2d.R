#' Flip a 2D cross section
#'
#' Flips a 2D cross section so that the left side becomes the right,
#' and the right becomes the left.
#'
#' @param xs2d Single 2D cross section
#' @returns The original 2D cross section with the left and right
#' sides switched. The distance values (along the cross section) are
#' flipped in sign.
flip_xs2d <- function(xs2d) {
  # Flip all coordinates
  xs2d$coordinates[, 1] <- -xs2d$coordinates[, 1]
  
  # Flip banks
  xs2d$banks <- sort(-xs2d$banks)
  
  # Flip thalwegs
  xs2d$thalwegs <- sort(-xs2d$thalwegs)
  
  xs2d
}
