coords_thalwegs <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  nodes <- coords_all(profile)
  nodes[nodes[, 2] == min(nodes[, 2]), , drop = FALSE]
  nodes
}
