#' Flip Planimetric (1D) Cross Sections
#'
#' Flips planimetric cross sections so that the right side becomes
#' the left, and the right becomes the left.
#'
#' @param sxc Planimetric cross section (sxc) object.
#' @returns The original cross section where each section is flipped,
#' as if rotating each cross section by 180 degrees.
flip_xs1d <- function(sxc) {
  checkmate::assert_class(sxc, "sxc")
  for (i in seq_along(sxc)) {
    n <- nrow(sxc[[i]])
    sxc[[i]][,] <- sxc[[i]][n:1, ]
  }
  sxc
}
