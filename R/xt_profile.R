#' Create profile cross section
#'
#' Create a profile cross section from coordinates and bank positions.
#' This function takes elevation data and bank positions and creates
#' an xs_profile object with left and right sides.
#'
#' @param coords Matrix of coordinates with columns (distance, elevation)
#' @param bankpoints Vector of distance values where banks occur, alternating between water and land
#' @returns An xs_profile object with left and right sides
#' @details This function creates a profile cross section by:
#' 1. Injecting bank points at the specified distances
#' 2. Finding thalwegs (lowest points) in the channel
#' 3. Splitting the profile into left and right sides
#' 4. Creating the xs_profile structure
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

  # For now, assume simple left-right structure (first and last bankpoints)
  # This could be extended to handle islands more complexly
  left_bank_distance <- bankpoints[1]
  right_bank_distance <- bankpoints[length(bankpoints)]

  # Subset to include only points between banks
  coords_subset <- coords[coords[, 1] >= left_bank_distance & coords[, 1] <= right_bank_distance, , drop = FALSE]

  # Find thalwegs in the channel
  thalwegs <- get_thalwegs(coords_subset)
  x_thalwegs <- thalwegs[, 1]

  # Split into left and right sides
  coords_left <- coords[coords[, 1] <= min(x_thalwegs), , drop = FALSE]
  coords_right <- coords[coords[, 1] >= max(x_thalwegs), , drop = FALSE]

  # Get bed coordinates (between banks)
  coords_left_bed <- coords_left[coords_left[, 1] >= left_bank_distance, , drop = FALSE]
  coords_right_bed <- coords_right[coords_right[, 1] <= right_bank_distance, , drop = FALSE]

  # Find thalwegs for each side
  left_thalwegs <- get_thalwegs(coords_left_bed)
  right_thalwegs <- get_thalwegs(coords_right_bed)

  # Create xs_profile structure
  profile <- list(
    left = list(
      coordinates = coords_left,
      bank_point = get_2d_points(coords_left, left_bank_distance),
      thalweg = left_thalwegs
    ),
    right = list(
      coordinates = coords_right,
      bank_point = get_2d_points(coords_right, right_bank_distance),
      thalweg = right_thalwegs
    )
  )
  structure(profile, class = "xs_profile")
}
