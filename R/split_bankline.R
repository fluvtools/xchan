#' Split a Bankline into Left and Right
#'
#' NOTE: double check the left and right banks, that they make sense,
#' as per the example.
#' I suspect this algorithm may fail in some cases.
#'
#' @param bankline Bankline polygon.
#' @param centerline A multiline object representing the channel
#' centerline. If `NULL` (the default), this is auto-generated using
#' the centerline package.
#' @returns A list of two multilines: entry `"left"` represents the left bank,
#' entry `"right"` represents the right bank. The original `bankline` object
#' could be retrieved by combining both multilines together.
#' @examples
#' lr <- split_bankline(demo_bankline)
#' plot(demo_bankline)
#' plot(lr$left, add = TRUE, col = "red")
#' plot(lr$right, add = TRUE, col = "blue")
split_bankline <- function(bankline, centerline = NULL) {
  if (is.null(centerline)) {
    centerline <- sf::st_geometry(centerline::cnt_path_guess(bankline, keep = 1))
  }

  # Get the intersection of the bankline and centerline
  intersection <- sf::st_intersection(bankline, centerline)

  # Convert the bankline boundary to a centerline
  polygon_boundary <- sf::st_cast(bankline, "MULTILINESTRING")

  # Split the bankline boundary by the intersection line
  split <- lwgeom::st_split(polygon_boundary, intersection)

  # Extract the resulting lines
  split_parts <- sf::st_collection_extract(split, "LINESTRING")

  # extend the bankline if split_parts has length of 1
  if (length(split_parts) < 2) {
    first_point <- as.numeric(centerline[[1]][1,])
    last_point <- as.numeric(centerline[[1]][nrow(centerline[[1]]),])

    direction <- last_point - first_point
    direction <- direction / sqrt(sum(direction^2))

    # Extend the points x/2 on each side
    len <- as.numeric(sf::st_length(centerline)[1])
    new_first_point <- first_point - (len / 2) * direction
    new_last_point <- last_point + (len / 2) * direction

    # Create the new extended line
    extended_line <- sf::st_sfc(sf::st_linestring(
      rbind(
        new_first_point,
        sf::st_coordinates(centerline),
        new_last_point)
      )
    )

    original_crs <- sf::st_crs(centerline)
    extended_line <- sf::st_set_crs(extended_line, original_crs)

    # Get the intersection of the bankline and centerline
    intersection <- sf::st_intersection(bankline, extended_line)

    # Convert the bankline boundary to a centerline
    polygon_boundary <- sf::st_cast(bankline, "MULTILINESTRING")

    # Split the bankline boundary by the intersection line
    split <- lwgeom::st_split(polygon_boundary, intersection)

    # Extract the resulting lines
    split_parts <- sf::st_collection_extract(split, "LINESTRING")

  }

  l <- as.numeric(sf::st_length(split_parts))
  to_combine <- l != max(l)

  # Combine them into multilinestrings if there are multiple lines per side
  left <- sf::st_combine(sf::st_geometry(split_parts[to_combine, ]))
  right <- sf::st_combine(sf::st_geometry(split_parts[!to_combine, ]))

  list(
    left = sf::st_cast(left, "MULTILINESTRING"),
    right = sf::st_cast(right, "MULTILINESTRING")
  )
}
