#' Dredge a channel using specified method
#'
#' @param channel Channel object
#' @param dredger Dredger method to use
#' @returns A modified channel object
#' @details
#' Dredging modifies the profile cross-sections of a channel to create a new
#' channel geometry. The dredger parameter specifies the dredging algorithm to use.
#' Different dredgers may require different parameters (e.g., depth and water surface
#' for rectangular dredging, DEM data for DEM-based dredging).
#' @examples
#' # Dredge to rectangular shape with default water surface
#' channel_dredged <- xt_dredge(channel, dredger = dredger_rectangle(depth = 5))
#'
#' # Dredge to rectangular shape with custom water surface
#' channel_dredged <- xt_dredge(channel, dredger = dredger_rectangle(depth = 5, wse = elevation_column("water_surface")))
#'
#' # Dredge using DEM method (when implemented)
#' channel_dredged <- xt_dredge(channel, dredger = dredger_dem(my_dem, sample_freq = 1))
#' @export
xt_dredge <- function(channel, dredger) {
  checkmate::assert_class(channel, "sxchan")

  # Handle text input by calling appropriate dredger function
  if (is.character(dredger)) {
    dredger_fun <- paste0("dredger_", dredger)
    dredger <- rlang::exec(dredger_fun)
  }

  # Execute the dredger function
  new_chan <- dredger(channel)
  checkmate::assert_class(channel, "sxchan")
  new_chan
}
