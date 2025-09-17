#' Create profile cross section
#'
#' Create a profile cross section from coordinates and bank positions.
#' This function takes elevation data and bank positions and creates
#' an xs_profile object with coordinates, banks, and thalwegs. Use this
#' function whenever making or modifying profile cross sections, because
#' it ensures standards are met.
#'
#' @param coords Matrix of coordinates with columns representing distance
#' along the cross section (1st column), and elevation (2nd column).
#' @param bankpoints Vector of distance values where banks occur, alternating
#' between water and land. Must have an even length (see details).
#' @returns An "xs_profile" object, which is a list of the following names:
#'
#' - `coordinates`: an `n` x 2 matrix of distances along the cross section
#'   (column 1) and elevation (column 2).
#' - `banks`: A numeric vector of even length representing the bankpoints,
#'   encoded as distances along the cross section,
#'   sorted from left to right. These points have alternating representations
#'   of land-to-water and water-to-land.
#' - `thalwegs`: A numeric vector representing the location of the thalwegs,
#'   encoded as distances along the cross section, sorted from left to right.
#'   These points are where the channel reaches minimum depth.
#' - `thalweg_elev`: A single numeric representing the elevation of the
#'   channel's thalweg; that is, the lowest point within the channel.
#' @details
#' The bankpoints vector must be a multiple of 2, alternating between water
#' and land to allow for islands. For example: c(-3, -2, 2, 3) means water
#' from -3 to -2, land from -2 to 2, water from 2 to 3.
#'
#' The following standards are ensured:
#'
#' 1. Distances along the cross section are shifted so that 0 represents the
#'    center point.
#' 2. Bank points and coordinates are sorted from left to right. In case of
#'    vertical cliffs having the same distance value, the order is preserved.
#'    Note that sorting means bank undercutting is currently not supported.
#' 3. Bank points are always present in the profile coordinates.
#' 4. Thalwegs are re-computed.
#'
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
  checkmate::assert_matrix(
    coords, mode = "numeric", any.missing = FALSE, ncols = 2
  )
  checkmate::assert_numeric(bankpoints, any.missing = FALSE, min.len = 2L)
  if (length(bankpoints) %% 2 != 0) {
    stop("bankpoints must have an even number of elements.")
  }

  # 1. Distances along the cross section are shifted so that 0 represents the
  #    center point.
  # The mean of the most extreme bankpoints represents the offset from 0 and
  # can be used as the shift quantity.
  shift <- mean(range(bankpoints))
  coords[, 1] <- coords[, 1] - shift
  bankpoints <- bankpoints - shift

  # 2. Bank points and coordinates are sorted from left to right. In case of
  #    vertical cliffs having the same distance value, the order is preserved.
  coords <- coords[order(coords[, 1]), ]
  bankpoints <- sort(bankpoints)

  # 3. Bank points are always present in the profile coordinates.
  coords <- inject_coords(coords, bankpoints)

  # 4. Thalwegs are re-computed.
  thalwegs <- coords_thalwegs(coords)
  thalweg_elev <- thalwegs[1, 2]
  x_thalwegs <- sort(thalwegs[, 1])

  profile <- list(
    coordinates = coords,
    banks = bankpoints,
    thalwegs = x_thalwegs,
    thalweg_elev = thalweg_elev
  )
  structure(profile, class = "xs_profile")
}
