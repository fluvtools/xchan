#' Demonstration cross section coordinates
#'
#'
#' @source The demo bankline polygon was generated using the following code
#' ```
#' # Load necessary libraries
#' library(sf)
#'
#' # Adjusted coordinates for the cross-section
#' demo_coords <- matrix(c(
#'   # Left side (steep descent into channel)
#'   -4, 11,
#'   -1, 10,    # left bank start (steep descent)
#'   1, 9,
#'   2, 7,     # left bank point
#'   4, 1,     # thalweg (deepest point of the channel)
#'
#'   # Right side (concave-up near channel, switches concavity toward floodplain)
#'   6, 1.5,   # concave-up section rising gradually
#'   9, 4,     # continuation of concave-up section
#'   12, 6,    # right bank point
#'   18, 8,    # flattening floodplain
#'   24, 8.5   # extended flat floodplain
#' ), ncol = 2, byrow = TRUE)
#' ```
"demo_coords"
