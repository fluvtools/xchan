#' Print method for xchan_tracer objects
#'
#' @param x A tracer object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.xchan_tracer <- function(x, ...) {
  cat("Tracer:", attr(x, "name"), "\n")
  
  # Show stored parameters
  params <- attr(x, "params")
  if (length(params) > 0) {
    cat("Parameters:\n")
    for (i in seq_along(params)) {
      param_name <- names(params)[i]
      param_value <- params[[i]]
      cat("  ", param_name, " = ", deparse(param_value), "\n", sep = "")
    }
  } else {
    cat("Parameters: none\n")
  }
  
  invisible(x)
}
