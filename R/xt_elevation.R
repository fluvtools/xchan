#' Get elevation values from a channel using elevation specification
#'
#' @param channel Channel object
#' @param reference Elevation specification
#' @returns A vector of elevation values for each cross-section
#' @details
#' This function extracts elevation values from a channel object using the specified
#' elevation reference. The elevation specification determines how elevation values
#' are calculated for each cross-section.
#' @examples
#' # Get thalweg elevations
#' elevations <- xt_elevation(channel, reference = elevation_thalweg())
#' 
#' # Get water surface elevations from a column
#' elevations <- xt_elevation(channel, reference = elevation_column("water_surface"))
#' 
#' # Get mean bank elevations
#' elevations <- xt_elevation(channel, reference = elevation_bank(.f = mean))
#' @export
xt_elevation <- function(channel, reference) {
  checkmate::assert_class(channel, "xchan")
  
  # Execute the elevation specification
  reference(channel)
}
