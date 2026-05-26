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
  bank_distances <- get_bank_distances(profile)
  bank_elev <- get_bank_elevations(profile)
  nodes <- coords_all(profile)
  nodes <- nodes[rev(seq_len(nrow(nodes))), , drop = FALSE]
  nodes[, 1] <- -nodes[, 1]

  out <- profile
  out$coordinates <- nodes
  flipped_bank_d <- sort(-bank_distances)
  flipped_bank_elev <- rev(bank_elev)
  out$banks <- vapply(
    seq_along(flipped_bank_d),
    function(i) {
      profile_bank_index_at_distance(nodes, flipped_bank_d[i], flipped_bank_elev[i])
    },
    integer(1L)
  )
  out$thalweg_elev <- te_orig
  out$thalwegs <- thalweg_indices_from_banks(out$coordinates, flipped_bank_d)
  out
}
