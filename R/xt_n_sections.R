#' Get the number of cross sections
#'
#' A generic function to determine the number of cross sections in an object.
#'
#' @param channel A channel object (`xchan_tbl`) or cross-section geometry
#'   container (`xchan`).
#' @return A single integer representing the number of cross sections.
#' @export
xt_n_sections <- function(channel) {
  if (!inherits(channel, "xchan_tbl") && !inherits(channel, "xchan")) {
    stop("`channel` must inherit from `xchan_tbl` or `xchan`.", call. = FALSE)
  }
  if (is.data.frame(channel)) {
    return(nrow(channel))
  }
  length(channel)
}
