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
  checkmate::assert_class(profile, "xs_profile")
  te_orig <- profile$thalweg_elev
  # Flip all distance coordinates
  nodes <- coords_all(profile)
  nodes[, 1] <- -nodes[, 1]
  # Flip bank distances
  bank_distances <- get_bank_distances(profile)
  banks <- sort(-bank_distances)
  out <- new_profile(nodes, bankpoints = banks)
  # Mirror does not change elevations; rebuilding via new_profile() / inject_coords()
  # can shift min(z) (interpolation, float equality on thalweg rows).
  out$thalweg_elev <- te_orig
  out$thalwegs <- which.min(abs(out$coordinates[, 2] - te_orig))
  out
}
