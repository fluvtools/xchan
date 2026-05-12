#' @exportS3Method base::print
print.xchan <- function(x, ...) {
  n <- length(x)
  cat("xchan geometry with", n, "cross sections.\n")
  crs <- xchan_crs(x)
  if (!is.na(crs)) {
    cat("CRS:", crs$input, "\n")
  }
  if (!is.null(attr(x, "axis", exact = TRUE))) {
    cat("Channel axis: stored (see xt_axis())\n")
  }
  if (n > 0L && xt_has_profile(x)) {
    cat("With profile view\n")
  }
  invisible(x)
}
