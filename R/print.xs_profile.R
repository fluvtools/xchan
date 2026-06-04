#' Print method for xs_profile objects
#'
#' @param x An xs_profile object
#' @param ... Additional arguments (ignored)
#' @examples
#' channel <- xt_as_channel(rep(1, 3))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' print(channel[[1]]$profile)
#' @exportS3Method base::print
print.xs_profile <- function(x, ...) {
  cat("xchan profile cross section\n")
  cat("  Coordinates:", nrow(x$coordinates), "points\n")
  cat("  Banks:", length(x$banks), "bank points\n")
  cat("  Thalwegs:", length(x$thalwegs), "thalweg points\n")

  # Show coordinate range
  if (nrow(x$coordinates) > 0) {
    cat("  Distance range:", range(x$coordinates[, 1]), "\n")
    cat("  Elevation range:", range(x$coordinates[, 2]), "\n")
  }

  invisible(x)
}
