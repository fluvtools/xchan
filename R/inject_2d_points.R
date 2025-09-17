#' Inject points into a 2D cross section matrix
#'
#' Inject a point into a 2D cross section matrix,
#' potentially splitting a linesegment into two
#' if x doesn't already land on a node.
#'
#' @param mat Matrix of nodes of distances along the cross section
#' (column one) and elevation (column two).
#' @param x Numeric vector; distance along cross section to add a new node to.
#' @returns The original matrix of nodes, with a additional
#' nodes correspoding to `x`, with linearly interpolated elevation.
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


inject_coords_legacy <- function(mat, x) {
  x_mat <- mat[, 1]
  x <- x[!(x %in% x_mat)]
  if (length(x) == 0) return(mat)
  new_points <- coords_interpolate(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat[order(new_mat[, 1]), ]
}

eval_new_coords <- function(mat, x) {
  x_mat <- mat[, 1]
  x <- x[!(x %in% x_mat)]
  if (length(x) == 0) return(mat)
  new_points <- coords_interpolate(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat[order(new_mat[, 1]), ]
}
