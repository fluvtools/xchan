#' Print method for xs_profile objects
#'
#' @param x An xs_profile object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.xs_profile <- function(x, ...) {
  cat("sxchan profile cross section\n")
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
