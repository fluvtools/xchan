#' Print method for xchan_elevation objects
#'
#' @param x An elevation object
#' @param ... Additional arguments (ignored)
#' @examples
#' print(elevation_thalweg())
#' @exportS3Method base::print
print.xchan_elevation <- function(x, ...) {
  cat("Elevation:", attr(x, "name"), "\n")

  # Show stored parameters
  params <- attr(x, "params")
  if (length(params) > 0) {
    cat("Parameters:\n")
    for (i in seq_along(params)) {
      param_name <- names(params)[i]
      param_value <- params[[i]]
      if (param_name == "...") {
        cat("  ... = ", deparse(param_value), "\n", sep = "")
      } else {
        cat("  ", param_name, " = ", deparse(param_value), "\n", sep = "")
      }
    }
  } else {
    cat("Parameters: none\n")
  }

  invisible(x)
}
