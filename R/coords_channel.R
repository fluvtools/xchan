coords_all <- function(profile) {
  checkmate::check_class(profile, "xs_profile")
  profile$coordinates
}
