#' Print method for xchan_side objects
#'
#' Prints the left/right proportions only (constructors [side_left()], etc., are equivalent).
#'
#' @param x A side object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.xchan_side <- function(x, ...) {
  cat("Side allocation specification.\n")
  cat("Proportions:\n")
  cat("  left =", x$left, "\n")
  cat("  right =", x$right, "\n")
  invisible(x)
}
