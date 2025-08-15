get_thalwegs <- function(mat) {
  mat[mat[, 2] == min(mat[, 2]), , drop = FALSE]
}
