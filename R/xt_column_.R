#' Plan/profile helpers for [`xchan`] objects
#'
#' @name channel_views
#' @keywords internal
#' @noRd
NULL

#' @noRd
channel_profile <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  assert_xchan_profile_homogeneity(channel)
  prof <- lapply(channel, function(xs) xs$profile)
  if (all(vapply(prof, is.null, logical(1)))) {
    return(NULL)
  }
  prof
}

#' @noRd
channel_plan <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  geoms <- lapply(channel, xsection_to_linestring)
  cr <- xchan_crs(channel)
  crs_use <- if (inherits(cr, "crs")) {
    cr
  } else {
    suppressWarnings(sf::st_crs(cr))
  }
  if (inherits(crs_use, "crs") && is.na(crs_use)) {
    crs_use <- sf::NA_crs_
  }
  sf::st_sfc(geoms, crs = crs_use)
}

#' @noRd
set_channel_profile <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  if (is.null(value)) {
    out <- xchan_with_profile(channel, NULL)
    validate_plan_profile_widths(out)
    return(out)
  }
  if (!is.list(value)) {
    stop("Profile value must be a list")
  }
  profile_classes <- vapply(value, inherits, logical(1), "xs_profile")
  if (!all(profile_classes)) {
    invalid_indices <- which(!profile_classes)
    stop(
      "All profile entries must be xs_profile objects. Invalid entries at indices: ",
      paste(invalid_indices, collapse = ", ")
    )
  }
  out <- xchan_with_profile(channel, value)
  validate_plan_profile_widths(out)
  out
}

#' @noRd
set_channel_plan <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  if (is.null(value)) {
    stop(
      "Cannot remove planimetric cross sections from a channel.",
      call. = FALSE
    )
  }

  validation_result <- validate_plan(value)
  if (!validation_result$valid) {
    stop(
      "Invalid plan view cross sections: ",
      paste(validation_result$issues, collapse = "; ")
    )
  }
  out <- xchan_with_plan(channel, value)
  validate_plan_profile_widths(out)
  out
}
