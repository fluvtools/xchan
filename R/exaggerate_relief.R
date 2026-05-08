#' Exaggerate relief in profile cross sections
#'
#' Applies a vertical exaggeration factor to profile elevations, measured as
#' height above each profile's minimum elevation (thalweg baseline). Use this
#' to improve visual interpretation in profile and 3D views.
#'
#' @param x An `xs_profile` object or a `xchan` object with a profile column.
#' @param times Single non-negative numeric exaggeration factor. Values above
#'   1 increase vertical relief; values between 0 and 1 compress it.
#' @param ... Unused.
#' @returns Object of the same class as `x`, with exaggerated profile
#'   elevations.
#' @examples
#' # xs <- xt_profile(matrix(c(-2, 10, 0, 8, 2, 10), ncol = 2, byrow = TRUE), c(-1, 1))
#' # xt_exaggerate_relief(xs)
#' @export
xt_exaggerate_relief <- function(x, times = 2, ...) {
  UseMethod("xt_exaggerate_relief")
}

#' @export
xt_exaggerate_relief.xs_profile <- function(x, times = 2, ...) {
  checkmate::assert_class(x, "xs_profile")
  checkmate::assert_number(times, lower = 0, finite = TRUE)

  ymin <- min(x$coordinates[, 2])
  x$coordinates[, 2] <- ymin + times * (x$coordinates[, 2] - ymin)
  x
}

#' @export
xt_exaggerate_relief.xchan <- function(x, times = 2, ...) {
  checkmate::assert_class(x, "xchan")
  checkmate::assert_number(times, lower = 0, finite = TRUE)
  profile <- xt_column_profile(x)
  if (is.null(profile)) {
    stop("`xt_exaggerate_relief()` requires a channel with profile cross sections.", call. = FALSE)
  }
  xt_column_profile(x) <- lapply(
    profile,
    function(p) xt_exaggerate_relief(p, times = times)
  )
  x
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

