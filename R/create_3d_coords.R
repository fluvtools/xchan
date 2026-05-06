#' Create 3D coordinates by mapping profile distances to plan positions
#'
#' Maps profile elevation data to plan view coordinates to create true
#' 3D geometries. Profile distances (column 1) are used to map
#' elevations (column 2) to the correct x-y positions along the plan
#' view cross section.
#'
#' @param plan Planimetric cross sections.
#' @param profile Profile cross sections.
#' @returns Matrix with 3D coordinates (x, y, z) where x,y come from
#' plan interpolation and z comes from profile elevation data
#' @details
#' Distances in profile_data are signed: negative for left side,
#' positive for right side.
#' The function assumes that the plan length equals the bank-to-bank
#' river span. First point in plan corresponds to left bank,
#' last point corresponds to right bank.
#' @examples
#' # plan_coords: matrix with x,y coordinates from plan view
#' # profile_data: matrix with distance,elevation from profile
#' # coords_3d <- create_3d_coords(plan_coords, profile_data)
create_3d_coords <- function(plan, profile) {
  # Get distances and elevations from profile
  banks_x <- profile$banks[1]
  coords <- profile$coordinates
  coords <- inject_coords(coords, banks_x)
  coords <- coords[coords[, 1] >= min(banks_x), , drop = FALSE]
  coords <- coords[coords[, 1] <= max(banks_x), , drop = FALSE]
  distances <- coords[, 1]
  distances <- distances - min(distances) # 0 = left bank.
  elevations <- coords[, 2]

  # Get starting and ending (x, y) from plan
  plan_coords <- sf::st_coordinates(plan)
  x0 <- plan_coords[1, "X"] # left bank
  y0 <- plan_coords[1, "Y"] # left bank
  x1 <- plan_coords[2, "X"]
  y1 <- plan_coords[2, "Y"]

  # Get the rise and run of the plan line, and the total length.
  rise <- y1 - y0
  run <- x1 - x0
  plan_length <- sqrt(rise^2 + run^2)

  # Profile now tells us where to put each elevation along the plan line:
  # To turn a "distance from left bank" d into an (x, y) coordinate, first find
  # how far along the cross section it is, as a proportion. This is the
  # same proportion of x and y gain relative to the full run and rise.
  prop <- distances / plan_length
  x <- x0 + prop * run
  y <- y0 + prop * rise

  xyz <- cbind(x = x, y = y, z = elevations)

  # Turn into an sf multilinestring geometry
  sf::st_multilinestring(list(xyz))
}
