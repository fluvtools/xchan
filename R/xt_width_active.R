#' Active Cross Section Widths
#'
#' Calculate the "active" width of channel cross sections, where an "active" width
#' is the width of the cross section occupied by water, not land. This is relevant when
#' there are islands / bars within the river.
#'
#' @param sxc A list of planimetric (1-dimensional) cross sections; i.e., and object of
#' class `"sxc"`.
#' @returns A numeric vector of length equal to the length of the input `sxc` containing the
#' active widths of each cross section.
#' @export
xt_width_active <- function(sxc) {
  checkmate::assert_class(sxc, "sxc")
  if (length(sxc) == 0) return(sxc)
  widths <- numeric()
  for (i in seq_along(sxc)) {
    line <- sxc[[i]]
    coords <- sf::st_coordinates(line)
    distances <- sqrt(diff(coords[, 1])^2 + diff(coords[, 2])^2)
    n_sections <- length(distances)
    if (n_sections %% 2 != 1) {
      stop(
        "Cross section number ", i, " contains ", length(distances),
        " sections.\n",
        "Expecting alternating water and land, and therefore an odd ",
        "number of sections."
      )
    }
    ind <- seq_len((n_sections + 1) / 2) * 2 - 1
    widths[i] <- sum(distances[ind])
  }
  widths
}
