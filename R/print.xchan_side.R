#' Print method for xchan_side objects
#'
#' @param x A side object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.xchan_side <- function(x, ...) {
  cat("Side:", attr(x, "name"), "\n")
  cat("Proportions:\n")
  cat("  left =", x$left, "\n")
  cat("  right =", x$right, "\n")
  invisible(x)
}
