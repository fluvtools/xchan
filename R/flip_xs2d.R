#' Flip a 2D cross section
#'
#' Flips a 2D cross section so that the left side becomes the right,
#' and the right becomes the left.
#'
#' @param xs2d Single 2D cross section
#' @returns The original 2D cross section with the left and right
#' sides switched. The distance values (along the cross section) are
#' flipped in sign.
#' @export
flip_xs2d <- function(xs2d) {
  names(xs2d) <- rev(names(xs2d))
  xs2d$left$multiline[, 1]  <- -xs2d$left$multiline[, 1]
  xs2d$left$bank[1]         <- -xs2d$left$bank[1]
  xs2d$left$thalweg[1]      <- -xs2d$left$thalweg[1]
  xs2d$right$multiline[, 1] <- -xs2d$right$multiline[, 1]
  xs2d$right$bank[1]        <- -xs2d$right$bank[1]
  xs2d$right$thalweg[1]     <- -xs2d$right$thalweg[1]
  xs2d
}

#' Flip Planimetric (1D) Cross Sections
#'
#' Flips planimetric cross sections so that the right side becomes
#' the left, and the right becomes the left.
#'
#' @param sxc Planimetric cross section (sxc) object.
#' @returns The original cross section where each section is flipped,
#' as if rotating each cross section by 180 degrees.
#' @export
flip_sxc <- function(sxc) {
  checkmate::assert_class(sxc, "sxc")
  for (i in seq_along(sxc)) {
    n <- nrow(sxc[[i]])
    sxc[[i]][,] <- sxc[[i]][n:1, ]
  }
  sxc
}
