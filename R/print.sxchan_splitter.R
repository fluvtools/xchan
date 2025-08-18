#' @export
print.sxchan_splitter <- function(x, ...) {
  cat("Erosion splitter function\n")
  print(environment(x))
  nm <- attributes(x)[["name"]]
  if (!is.null(nm)) {
    cat("Splitter:", nm, "\n")
  }
}
