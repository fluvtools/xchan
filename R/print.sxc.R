#' @exportS3Method base::print
print.sxc <- function(x, ...) {
  n <- length(x)
  if (n == 1) {
    s <- ""
  } else {
    s <- "s"
  }
  cat(
    "Simple cross section collection with ", n, " cross section", s, "\n",
    sep = ""
  )
  NextMethod()
}
