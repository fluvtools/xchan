#' Calculate XS width when there are islands or not
#'

#' @export
xs_width_multi <- function(xs1d, xs2d) {

}

#' Workhorse for the `xs_width_multi()` function.
#' Handles multiple cross sections; no need to run the function one at
#' a time.
#'
#' @param coords The second entry from the output of `get_islandxs()`;
#' A list of bank coordinates for each cross section.
#' @param XsecLine Planimetric (1D) cross sections; object of class `sxc`.
#' @returns A vector of widths, of length equal to the common lengths
#' of the inputs, `coords` and `XsecLine`.
xs_width_multi <- function(coords, XsecLine) {

  vctrs::vec_size_common(coords, XsecLine)

  # Ensure the line has a CRS
  if (is.null(sf::st_crs(XsecLine))) {
    sf::st_crs(XsecLine) <- 3157  # Assign CRS if missing
  }

  # Ensure sampled_points has the same CRS
  bank_points <- sf::st_multipoint(as.matrix(coords[, c("X", "Y")]))
  bank_points <- sf::st_sfc(bank_points, crs = 3157)  # Convert to `sfc` with CRS

  # Convert all points into a valid sf object
  bank_points_sf <- sf::st_as_sf(
    data.frame(id = 1:length(bank_points)),
    geometry = sf::st_sfc(bank_points, crs = sf::st_crs(XsecLine[[1]]))
  )
  coords_matrix <- as.matrix(sf::st_coordinates(bank_points_sf)[, 1:2])

  # Create dataframe
  cross_section <- data.frame(
    x = coords_matrix[, 1],
    y = coords_matrix[, 2]
  )

  cross_section <- cross_section |>
    dplyr::mutate(
      distance = sqrt((x - dplyr::lag(x))^2 + (y - dplyr::lag(y))^2)
    )

  # Compute channel widths
  # Compute the Euclidean distance between consecutive points
  distances <- as.numeric(unlist(na.omit(cross_section$distance)))

  # Get number of distances
  n <- length(distances)

  # Initialize labels
  if (n == 1) {
    names(distances) <- c("Channel Width")
  } else if (n == 2) {
    names(distances) <- c("Left Channel Width", "Right Channel Width")
  } else {
    labels <- c("Left Channel Width")  # First is always left channel width

    # Assign alternating "Inner Channel Width" and "Inner Island Width"
    for (i in 2:(n-1)) {
      if (i %% 2 == 0) {
        labels <- c(labels, "Inner Island Width")
      } else {
        labels <- c(labels, "Inner Channel Width")
      }
    }

    labels <- c(labels, "Right Channel Width")  # Last is always right channel width

    names(distances) <- labels
  }
  as.list(distances)
}

