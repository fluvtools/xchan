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
inject_2d_points <- function(mat, x) {
  x_mat <- mat[, 1]
  x <- x[!(x %in% x_mat)]
  if (length(x) == 0) return(mat)
  new_points <- get_2d_points(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat[order(new_mat[, 1]), ]
}
