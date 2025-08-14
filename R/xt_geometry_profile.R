#' Extract or replace the profile or plan geometry in a cross-section object
#'
#' This function allows you to extract or replace the profile or plan geometry
#' in a cross-section object of class `sx`. The profile geometry is
#' the 2D representation of the cross-section, while the plan geometry is
#' the 1D representation.

#' @param cross_sections An object of class `sx` containing cross-section data.
#' @returns A cross-section object of class `sx` with the specified geometry
#' replaced. Or, if no replacement is specified, the function returns the
#' requested geometry of class `sx_2d` for profile or `sx_1d` for plan.
#' @export
xt_geometry_profile <- function(cross_sections) {
  checkmate::assert_class(cross_sections, "sx")
  profile_col <- attributes(cross_sections)$profile_col
  cross_sections[[profile_col]]
}

#' @export
xt_geometry_plan <- function(cross_sections) {
  checkmate::assert_class(cross_sections, "sx")
  plan_col <- attributes(cross_sections)$plan_col
  cross_sections[[plan_col]]
}

#' @export
`xt_geometry_profile<-` <- function(x, value) {
  checkmate::assert_class(x, "sx")
  checkmate::assert_class(value, "sx_2d")
  profile_col <- attributes(x)$profile_col
  x[[profile_col]] <- value
  x
}

#' @export
`xt_geometry_plan<-` <- function(x, value) {
  checkmate::assert_class(x, "sx")
  checkmate::assert_class(value, "sx_1d")
  plan_col <- attributes(x)$plan_col
  x[[plan_col]] <- value
  x
}
