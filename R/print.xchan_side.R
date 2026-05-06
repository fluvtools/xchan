#' Print method for xchan_side objects
#'
#' @param x A side object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.xchan_side <- function(x, ...) {
  cat("Side:", attr(x, "name"), "\n")

  # Get function arguments
  args <- formals(x)
  if (length(args) > 0) {
    cat("Parameters:\n")
    for (i in seq_along(args)) {
      arg_name <- names(args)[i]
      arg_value <- args[[i]]
      if (is.name(arg_value) && arg_value == "") {
        cat("  ", arg_name, " (required)\n", sep = "")
      } else {
        cat("  ", arg_name, " = ", deparse(arg_value), "\n", sep = "")
      }
    }
  }

  invisible(x)
}
