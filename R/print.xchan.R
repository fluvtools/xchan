#' @exportS3Method base::print
print.xchan_tbl <- function(x, ...) {
  n <- xt_n_sections(x)
  cat("xchan_tbl with", n, "cross sections.\n")
  xcol <- attr(x, "xsection_col", exact = TRUE)
  if (!is.null(xcol)) {
    cat("Cross-section column:", xcol, "\n")
    crs <- xchan_crs(x[[xcol]])
    if (!is.na(crs)) {
      cat("CRS:", crs$input, "\n")
    }
  }
  if (xt_has_profile(x)) {
    cat("With profile view\n")
  }
  NextMethod()
}

#' @exportS3Method base::print
print.xchan <- function(x, ...) {
  n <- length(x)
  cat("xchan geometry with", n, "cross sections.\n")
  crs <- xchan_crs(x)
  if (!is.na(crs)) {
    cat("CRS:", crs$input, "\n")
  }
  invisible(x)
}
