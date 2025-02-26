#' @export
xs_width <- function(xs) {
  xs$right$bank[1] - xs$left$bank[1]
}

#' @export
lb_width <- function(xs) {
  xs$left$thalweg[1] - xs$left$bank[1]
}

#' @export
rb_width <- function(xs) {
  xs$right$bank[1] - xs$right$thalweg[1]
}

#' @export
compute_channel_widths <- function(df) {
  # Compute the Euclidean distance between consecutive points
  distances <- as.numeric(unlist(na.omit(df$distance)))
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

#' @export

xs_width_multi <- function(coords, XsecLine) {

  # Ensure the line has a CRS
  if (is.null(st_crs(XsecLine))) {
    st_crs(XsecLine) <- 3157  # Assign CRS if missing
  }

  # Ensure sampled_points has the same CRS
  bank_points <- st_multipoint(as.matrix(coords[, c("X", "Y")]))
  bank_points <- st_sfc(bank_points, crs = 3157)  # Convert to `sfc` with CRS

  # Convert all points into a valid sf object
  bank_points_sf <- st_as_sf(data.frame(id = 1:length(bank_points)), geometry = st_sfc(bank_points, crs = st_crs(XsecLine)))
  coords_matrix <- as.matrix(st_coordinates(bank_points_sf)[, 1:2])
  # Extract elevation values from the DEM at these coordinates
  # Create dataframe
  cross_section <- data.frame(
    x = coords_matrix[, 1],
    y = coords_matrix[, 2]
  )
  cross_section <- cross_section |>
    mutate(
      distance = sqrt((x - lag(x))^2 + (y - lag(y))^2)
    )

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

  # Compute channel widths
  channel_widths <- as.list(distances)
  return(channel_widths)
}
