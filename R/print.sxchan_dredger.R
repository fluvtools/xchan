#' Print method for sxchan_dredger objects
#'
#' @param x A dredger object
#' @param ... Additional arguments (ignored)
#' @exportS3Method base::print
print.sxchan_dredger <- function(x, ...) {
  cat("Dredger:", attr(x, "name"), "\n")
  
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
