#' Print method for sxchan_splitter objects
#'
#' @param x A splitter object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.sxchan_splitter <- function(x, ...) {
  cat("Splitter:", attr(x, "name"), "\n")
  
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
