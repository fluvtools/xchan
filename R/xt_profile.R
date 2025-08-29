#' Create profile cross section
#'
#' Create a profile cross section from coordinates and bank positions.
#' This function takes elevation data and bank positions and creates
#' an xs_profile object with coordinates, banks, and thalwegs.
#'
#' @param coords Matrix of coordinates with columns (distance, elevation)
#' @param bankpoints Vector of distance values where banks occur, alternating between water and land
#' @returns An xs_profile object with coordinates, banks, and thalwegs
#' @details This function creates a profile cross section by:
#' 1. Injecting bank points at the specified distances
#' 2. Finding thalwegs (lowest points) in the channel
#' 3. Creating the xs_profile structure with coordinates, banks, and thalwegs
#'
#' The bankpoints vector must be a multiple of 2, alternating between water and land
#' to allow for islands. For example: c(-3, -2, 2, 3) means water from -3 to -2,
#' land from -2 to 2, water from 2 to 3.
#' @examples
#' # Create a profile cross section
#' coords <- matrix(c(
#'   -5, 10,  # left side
#'   -2, 8,
#'   0, 5,   # center (thalweg)
#'   2, 8,
#'   5, 10   # right side
#' ), ncol = 2, byrow = TRUE)
#'
#' # Simple channel: water from -3 to 3
#' profile <- xt_profile(coords, bankpoints = c(-3, 3))
#'
#' # Channel with island: water from -3 to -1, land from -1 to 1, water from 1 to 3
#' profile <- xt_profile(coords, bankpoints = c(-3, -1, 1, 3))
#' @export
xt_profile <- function(coords, bankpoints) {
  # Validate bankpoints
  checkmate::assert_numeric(bankpoints, any.missing = FALSE)
  if (length(bankpoints) %% 2 != 0) {
    stop("bankpoints must have an even number of elements")
  }
  if (length(bankpoints) < 2) {
    stop("bankpoints must have at least 2 elements")
  }

  # Inject bank points at specified distances
  coords <- inject_2d_points(coords, bankpoints)

  # Find thalwegs (lowest points) in the entire profile
  thalwegs <- get_thalwegs(coords)
  x_thalwegs <- thalwegs[, 1]

  # Create xs_profile structure with new format
  profile <- list(
    coordinates = coords,
    banks = bankpoints,
    thalwegs = x_thalwegs
  )
  structure(profile, class = "xs_profile")
}
