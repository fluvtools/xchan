#' Split a Bankline into Left and Right
#'
#' NOTE: double check the left and right banks, that they make sense,
#' as per the example.
#' I suspect this algorithm may fail in some cases.
#'
#' @param bankline Bankline polygon.
#' @param centerline A multiline object representing the channel
#' centerline. If `NULL` (the default), this is auto-generated using
#' `xt_generate_centerline()`.
#' @returns A list of two multilines: entry `"left"` represents the left bank,
#' entry `"right"` represents the right bank. The original `bankline` object
#' could be retrieved by combining both multilines together.
#' @examples
#' cl <- xt_generate_centerline(demo_bankline)
#' lr <- split_bankline(demo_bankline, cl)
#' plot(demo_bankline)
#' plot(cl, add = TRUE)
#' plot(lr$left, add = TRUE, col = "red")
#' plot(lr$right, add = TRUE, col = "blue")
#' @export
split_bankline <- function(bankline, centerline = NULL) {
  if (is.null(centerline)) {
    centerline <- xt_generate_centerline(bankline)
  }

  # Get the intersection of the bankline and centerline
  intersection <- sf::st_intersection(bankline, centerline)

  # Convert the bankline boundary to a centerline
  polygon_boundary <- sf::st_cast(bankline, "MULTILINESTRING")

  # Split the bankline boundary by the intersection line
  split <- lwgeom::st_split(polygon_boundary, intersection)

  # Extract the resulting lines
  split_parts <- sf::st_collection_extract(split, "LINESTRING")

  l <- vapply(split_parts, sf::st_length, FUN.VALUE = numeric(1L))
  to_combine <- l != max(l)

  # Combine them into multilinestrings if there are multiple lines per side
  left <- sf::st_combine(split_parts[to_combine])
  right <- sf::st_combine(split_parts[!to_combine])

  list(
    left = sf::st_cast(left, "MULTILINESTRING"),
    right = sf::st_cast(right, "MULTILINESTRING")
  )
}
