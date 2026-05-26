#' Get the number of cross sections
#'
#' A generic function to determine the number of cross sections in an object.
#'
#' @param channel Cross-section geometry container ([`xchan`]).
#' @return A single integer representing the number of cross sections.
#' @export
xt_n_sections <- function(channel) {
  if (!inherits(channel, "xchan")) {
    stop("`channel` must be an `xchan` object.", call. = FALSE)
  }
  length(channel)
}
