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
  mat <- coords_all(profile)
  x_mat <- mat[, 1]
  x <- unique(x[!(x %in% x_mat)])
  if (length(x) == 0) {
    return(profile)
  }
  new_points <- coords_interpolate(profile, x)
  new_mat <- rbind(mat, new_points)
  xt_profile(new_mat, bankpoints = profile$banks)
}
