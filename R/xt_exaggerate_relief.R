#' Exaggerate relief in profile cross sections
#'
#' Applies a vertical exaggeration factor to profile elevations, measured as
#' height above each profile's minimum elevation (thalweg baseline). Use this
#' to improve visual interpretation in profile and 3D views.
#'
#' @param x An `xs_profile` or [`xchan`] object with profile geometry.
#' @param times Single non-negative numeric exaggeration factor. Values above
#'   1 increase vertical relief; values between 0 and 1 compress it.
#' @param ... Must be empty.
#' @returns Object of the same class as `x`, with exaggerated profile
#'   elevations.
#' @examples
#' channel <- xt_as_channel(rep(1, 6))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' profile_object <- channel[[1]]$profile
#' xt_exaggerate_relief(profile_object)
#' @export
xt_exaggerate_relief <- function(x, times = 2, ...) {
  UseMethod("xt_exaggerate_relief")
}

#' @export
xt_exaggerate_relief.xs_profile <- function(x, times = 2, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(x, "xs_profile")
  checkmate::assert_number(times, lower = 0, finite = TRUE)

  ymin <- min(x$coordinates[, 2])
  x$coordinates[, 2] <- ymin + times * (x$coordinates[, 2] - ymin)
  x
}

#' @export
xt_exaggerate_relief.xchan <- function(x, times = 2, ...) {
  rlang::check_dots_empty()
  checkmate::assert_class(x, "xchan")
  checkmate::assert_number(times, lower = 0, finite = TRUE)
  if (!xt_has_profile(x)) {
    stop(
      "`xt_exaggerate_relief()` requires a channel with profile cross sections.",
      call. = FALSE
    )
  }
  out <- x
  for (i in seq_along(out)) {
    if (is.null(out[[i]]$profile)) {
      next
    }
    out[[i]]$profile <- xt_exaggerate_relief(out[[i]]$profile, times = times)
  }
  out
}

#' @exportS3Method xt_exaggerate_relief default
xt_exaggerate_relief.default <- function(x, times = 2, ...) {
  stop(
    "No `xt_exaggerate_relief()` method for class ",
    paste(class(x), collapse = "/"),
    ". Use an `xs_profile` or `xchan` object.",
    call. = FALSE
  )
}
