#' Plan/profile helpers for channel objects
#'
#' Access planimetric and profile cross-section views from a channel object.
#' Channel rows store one `xsection` object in the active cross-section column.
#'
#' @name channel_views
#' @param channel An object of class `xchan_tbl`.
#' @param value For profile, a list of `xs_profile` objects (or `NULL` to drop
#'   profile from all sections); for plan, an `sfc_LINESTRING` object.
#' @returns Derived plan/profile views, or the updated channel on assignment.
#' @keywords internal
#' @noRd
NULL

#' Extract or replace plan/profile views from channel sections
#'
#' Channel rows store one `xsection` object in the active cross-section column.
#' `channel_plan()` and `channel_profile()` provide derived plan/profile views
#' from those `xsection` objects.
#'
#' @param channel An object of class `xchan_tbl`.
#' @param value For profile, a list of `xs_profile` objects (or `NULL` to drop
#'   profile from all sections); for plan, an `sfc_LINESTRING` object.
#' @returns Derived plan/profile views, or the updated channel on assignment.
#' @noRd
get_xsection_col <- function(channel) {
  col <- attr(channel, "xsection_col", exact = TRUE)
  if (!is.null(col) && col %in% names(channel)) {
    return(col)
  }
  NULL
}

#' @noRd
channel_profile <- function(channel) {
  checkmate::assert_class(channel, "xchan_tbl")
  xcol <- get_xsection_col(channel)
  if (!is.null(xcol)) {
    return(xchan_to_profile(channel[[xcol]]))
  }
  NULL
}

#' @noRd
channel_plan <- function(channel) {
  checkmate::assert_class(channel, "xchan_tbl")
  xcol <- get_xsection_col(channel)
  if (!is.null(xcol)) {
    return(xchan_to_plan(channel[[xcol]]))
  }
  NULL
}

#' @noRd
set_channel_profile <- function(channel, value) {
  checkmate::assert_class(channel, "xchan_tbl")
  xcol <- get_xsection_col(channel)
  if (is.null(xcol)) {
    stop("Profile replacement requires channel with `xsection` column.", call. = FALSE)
  }
  if (is.null(value)) {
    channel[[xcol]] <- xchan_with_profile(channel[[xcol]], NULL)
    return(channel)
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
  channel[[xcol]] <- xchan_with_profile(channel[[xcol]], value)
  validate_plan_profile_widths(channel)
  channel
}

#' @noRd
set_channel_plan <- function(channel, value) {
  checkmate::assert_class(channel, "xchan_tbl")
  xcol <- get_xsection_col(channel)
  if (is.null(xcol)) {
    stop("Plan replacement requires channel with `xsection` column.", call. = FALSE)
  }
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
  channel[[xcol]] <- xchan_with_plan(channel[[xcol]], value)
  validate_plan_profile_widths(channel)
  channel
}
