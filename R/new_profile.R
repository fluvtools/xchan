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
#' @returns An `"xs_profile"` object: a list with the following components:
#'
#' - `coordinates`: an `n` x 2 matrix of distances along the cross section
#'   (column 1) and elevation (column 2).
#' - `banks`: A numeric vector of **even length** whose values are **row indices**
#'   into `coordinates` (profile coordinates along the cross section, not
#'   distance along the channel). Banks appear left-to-right in profile
#'   order and alternate land-to-water / water-to-land (including islands).
#'   Distances at bank vertices are `coordinates[banks, 1]`.
#' - `thalwegs`: A numeric vector of **row indices** into `coordinates` for
#'   thalweg point(s) (global minimum depth may include points outside the
#'   defined banks). Distances at thalweg vertices are `coordinates[thalwegs, 1]`.
#' - `thalweg_elev`: A single numeric: elevation at the lowest point in
#'   `coordinates` (used as the channel thalweg elevation).
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
#' @keywords internal
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
#' profile <- new_profile(coords, bankpoints = c(-3, 3))
#'
#' # Channel with island: water from -3 to -1, land from -1 to 1, water from 1 to 3
#' profile <- new_profile(coords, bankpoints = c(-3, -1, 1, 3))
new_profile <- function(coords, bankpoints) {
  checkmate::assert_matrix(
    coords,
    mode = "numeric",
    any.missing = FALSE,
    ncols = 2
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

  # Convert bank distances to coordinate row indices.
  bank_idx <- vapply(
    bankpoints,
    function(b) which.min(abs(coords[, 1] - b)),
    integer(1)
  )

  # 4. Thalwegs are re-computed as coordinate row indices.
  thalweg_idx <- which(coords[, 2] == min(coords[, 2]))
  thalweg_elev <- min(coords[, 2])

  profile <- list(
    coordinates = coords,
    banks = bank_idx,
    thalwegs = thalweg_idx,
    thalweg_elev = thalweg_elev
  )
  structure(profile, class = "xs_profile")
}
