#' Assemble an [`xchan`] from plan linestrings (internal)
#'
#' Used by tests and coercion helpers. Validates plan widths against profiles
#' when profiles are present.
#'
#' @param l `sfc_LINESTRING` of cross sections.
#' @param axis Optional validated axis (`sfc` LINESTRING length 1), or `NULL`.
#' @param profile Optional list of `xs_profile` objects, same length as `l`.
#' @returns An [`xchan`] object.
#' @noRd
new_channel <- function(l, axis = NULL, profile = NULL) {
  if (!inherits(l, "sfc") || !inherits(l, "sfc_LINESTRING")) {
    stop("`new_channel()` expects an `sfc_LINESTRING`.", call. = FALSE)
  }
  vp <- validate_plan(l)
  if (!vp$valid) {
    stop(paste(vp$issues, collapse = "; "), call. = FALSE)
  }

  xsec <- xchan_from_plan_profile(l, profile)
  xsec <- `xchan_crs<-`(xsec, sf::st_crs(l))
  if (!is.null(axis)) {
    attr(xsec, "axis") <- axis
  }
  validate_plan_profile_widths(xsec)
  xsec
}
