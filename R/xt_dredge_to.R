#' Dredge profile cross sections to a target bathymetry
#'
#' @param channel An [`xchan`][xchan()] or [xsection] object with
#'   profile cross sections.
#' @param bathy A bathymetry specification from [bathy_rectangle()] or
#'   [bathy_vshape()].
#' @param ... Reserved for methods (must be empty).
#' @returns Object of the same class as `channel`, with profile beds adjusted
#'   toward the target bathymetry.
#'
#' @details
#' Dredging modifies every span between consecutive bank contacts on each
#' profile cross section to match the supplied bathymetry specification. Where
#' the existing bed is higher than the target, material is removed; where the
#' existing channel is deeper than the target, the bed is raised. Outer bank
#' positions and elevations are unchanged.
#'
#' Cross sections with mid-channel islands are dredged span by span: each water
#' interval and each island interior between consecutive bank contacts receives
#' the same target bathymetry.
#'
#' This is useful when profile cross sections were sampled from a DEM that does
#' not represent submerged topography (for example LIDAR): the river may appear
#' as a flat surface at bank elevation rather than a channel. Supplying
#' synthetic target bathymetry inserts a channel geometry for analysis.
#'
#' @examples
#' channel <- xt_as_channel(rep(10, 3))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' xt_dredge_to(channel, bathy = bathy_rectangle(depth = 2))
#'
#' @seealso [bathy_rectangle()], [bathy_vshape()], [elevation_bank()]
#' @export
xt_dredge_to <- function(channel, bathy, ...) {
  UseMethod("xt_dredge_to")
}

#' @rdname xt_dredge_to
#' @export
xt_dredge_to.xchan <- function(channel, bathy, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_class(bathy, "xchan_bathymetry")
  if (!xt_has_profile(channel)) {
    stop(
      "Channel must have profile cross sections to dredge.",
      call. = FALSE
    )
  }
  apply_bathymetry(channel, bathy)
}

#' @rdname xt_dredge_to
#' @export
xt_dredge_to.xsection <- function(channel, bathy, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xsection")
  checkmate::assert_class(bathy, "xchan_bathymetry")
  if (is.null(channel$profile)) {
    stop(
      "Cross section must have a profile view to dredge.",
      call. = FALSE
    )
  }
  cr <- attr(channel, "crs", exact = TRUE)
  crs_use <- if (!is.null(cr)) cr else sf::NA_crs_
  wrapped <- xchan(list(channel), crs = crs_use)
  dredged <- xt_dredge_to(wrapped, bathy)
  dredged[[1]]
}

#' @rdname xt_dredge_to
#' @exportS3Method xt_dredge_to default
xt_dredge_to.default <- function(channel, bathy, ...) {
  stop(
    "No `xt_dredge_to()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan` or `xsection` object.",
    call. = FALSE
  )
}
