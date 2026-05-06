#' Chainage column validation (internal)
#'
#' `chainage` is an optional numeric column: distance along the channel axis from
#' its upstream end to each section’s sampling station. Used to preserve
#' downstream order after `dplyr::arrange()` and related operations.
#'
#' @noRd
has_chainage_column <- function(channel) {
  if (!"chainage" %in% names(channel)) {
    return(FALSE)
  }
  ch <- channel[["chainage"]]
  is.numeric(ch) && length(ch) == nrow(channel) && !anyNA(ch)
}
