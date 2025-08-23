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
  names(xs2d) <- rev(names(xs2d))
  xs2d$left$coordinates[, 1]  <- -xs2d$left$coordinates[, 1]
  xs2d$left$bank_point[1]         <- -xs2d$left$bank_point[1]
  xs2d$left$thalweg[1]      <- -xs2d$left$thalweg[1]
  xs2d$right$coordinates[, 1] <- -xs2d$right$coordinates[, 1]
  xs2d$right$bank_point[1]        <- -xs2d$right$bank_point[1]
  xs2d$right$thalweg[1]     <- -xs2d$right$thalweg[1]
  xs2d
}
