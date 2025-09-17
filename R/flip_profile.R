#' Flip a profile cross section
#'
#' Flips a profile cross section so that the left side becomes the right,
#' and the right becomes the left.
#'
#' @param profile Single profile cross section
#' @returns The original profile cross section with the left and right
#' sides switched. The distance values (along the cross section) are
#' flipped in sign.
flip_profile <- function(profile) {
  checkmate::assert_class(profile, "sxchan_profile")
  # Flip all distance coordinates
  nodes <- coords_all(profile)
  nodes[, 1] <- -nodes[, 1]
  # Flip banks
  banks <- sort(-profile$banks)
  # Make new object
  xt_profile(nodes, bankpoints = banks)
}
