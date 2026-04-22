#' @exportS3Method base::print
print.xchan <- function(x, ...) {
  n <- xt_n_sections(x)
  cat("xchan channel with", n, "cross sections.\n")
  if (xt_has_plan(x)) {
    cat("With plan view\n")
  }
  if (xt_has_profile(x)) {
    cat("With profile view\n")
  }
  NextMethod()
}
