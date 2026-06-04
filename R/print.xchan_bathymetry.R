#' Print method for xchan_bathymetry objects
#'
#' @param x A bathymetry specification
#' @param ... Additional arguments (ignored)
#' @examples
#' print(bathy_rectangle(depth = 2))
#' @exportS3Method base::print
print.xchan_bathymetry <- function(x, ...) {
  rlang::check_dots_empty()
  cat("Bathymetry specification:", attr(x, "name"), "\n")

  params <- attr(x, "params")
  params <- params[names(params) != "wse"]
  if (length(params) > 0) {
    for (i in seq_along(params)) {
      param_name <- names(params)[i]
      param_value <- params[[i]]
      cat(
        "  * ",
        param_name,
        ": ",
        format_print_scalar(param_value),
        "\n",
        sep = ""
      )
    }
  }

  invisible(x)
}
