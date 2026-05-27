#' Full coordinate matrix of a profile cross section.
#' @noRd
coords_all <- function(profile) {
  checkmate::check_class(profile, "xs_profile")
  profile$coordinates
}

#' Inject points into a profile cross section matrix
#'
#' Inject a point into a 2D cross section matrix,
#' potentially splitting a linesegment into two
#' if x doesn't already land on a node.
#'
#' @param profile Cross section profile.
#' @param x Numeric vector; distance along cross section to add a new node to.
#' @returns The original profile cross section, with additional
#' nodes corresponding to `x`, with linearly interpolated elevation.
inject_coords <- function(profile, x) {
  is_profile <- inherits(profile, "xs_profile")
  if (is_profile) {
    mat <- coords_all(profile)
  } else {
    checkmate::assert_matrix(profile, mode = "numeric", ncols = 2)
    mat <- profile
  }
  x_mat <- mat[, 1]
  x <- unique(x[!(x %in% x_mat)])
  if (length(x) == 0) {
    return(mat)
  }
  new_points <- coords_interpolate(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat <- new_mat[order(new_mat[, 1]), , drop = FALSE]

  if (!is_profile) {
    return(new_mat)
  }

  bankpoints <- mat[profile$banks, 1]
  new_profile(new_mat, bankpoints = bankpoints)
}
