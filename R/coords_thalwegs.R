coords_thalwegs <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  t <- profile$thalwegs
  nodes <- coords_all(profile)
  nodes[t, , drop = FALSE]
}
