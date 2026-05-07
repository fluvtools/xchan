#' Sample DEM to generate profile cross sections
#'
#' Generate profile cross sections for a channel object using a digital
#' elevation model (DEM). This function samples the DEM along the planimetric
#' cross sections and creates xs_profile objects for each cross section.
#'
#' @param channel Channel object with planimetric cross sections.
#' @param dem Digital elevation model (raster or terra object).
#' @param ... Additional arguments (ignored).
#' @param extent_distance Distance to extend beyond banks; positive value.
#'   Plain numeric is interpreted in the channel's CRS length unit; a
#'   [units::units()] length object is converted automatically. Mutually
#'   exclusive with `extent_multiplier`.
#' @param extent_multiplier Multiplier of channel width to extend beyond banks;
#'   positive numeric value. Mutually exclusive with `extent_distance`.
#' @param sample_freq Distance between DEM sampling points; positive value.
#'   Same units treatment as `extent_distance`. Mutually exclusive with
#'   `sample_n`.
#' @param sample_n number to sample; positive integer value greater than 1.
#'   Mutually exclusive with `sample_freq`.
#' @returns Updated channel object with profile cross sections in the
#'   profile column.
#' @details This function extends the planimetric cross sections beyond the
#'   banks to create a "frame" for erosion analysis. The extent can be specified
#'   either as a fixed distance or as a multiplier of the channel width.
#'   Similarly, the sampling distance can be specified either as a fixed
#'   distance or as a multiplier of the channel width.
#'
#'   Sampling must remain inside DEM coverage. If any sampled point is outside
#'   the DEM extent, or if a sampled elevation is missing (`NA`), the function
#'   throws an error.
#' @examples
#' # Sample DEM with 50 m extension beyond banks and 1 m sample spacing
#' # channel_with_profiles <- xt_generate_profile(
#' #   channel, dem, extent_distance = 50, sample_freq = 1
#' # )
#'
#' # 2x channel width extension; sample count sets spacing along extended line
#' # channel_with_profiles <- xt_generate_profile(
#' #   channel, dem, extent_multiplier = 2, sample_n = 100
#' # )
#' @export
xt_generate_profile <- function(
  channel,
  dem,
  ...,
  extent_distance,
  extent_multiplier,
  sample_freq,
  sample_n
) {
  rlang::check_dots_empty()

  checkmate::assert_class(channel, "xchan")

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  # Validate extent parameters
  extent_specified <- sum(c(
    !missing(extent_distance),
    !missing(extent_multiplier)
  ))
  if (extent_specified != 1) {
    stop(
      "Exactly one of extent_distance or extent_multiplier must be specified"
    )
  }

  # Validate sampling parameters
  sample_specified <- sum(c(!missing(sample_freq), !missing(sample_n)))
  if (sample_specified != 1) {
    stop("Exactly one of sample_freq or sample_n must be specified")
  }

  # Convert any units inputs to bare numerics in the channel's CRS unit so
  # downstream coordinate arithmetic stays plain numeric.
  unit <- crs_length_unit(channel)
  if (!missing(extent_distance)) {
    extent_distance <- to_numeric_length(
      extent_distance,
      unit,
      arg = "extent_distance"
    )
  }
  if (!missing(sample_freq)) {
    sample_freq <- to_numeric_length(sample_freq, unit, arg = "sample_freq")
    checkmate::assert_number(sample_freq, lower = 0)
  }
  if (!missing(sample_n)) {
    checkmate::assert_count(sample_n)
    if (sample_n < 2) {
      stop("sample_n must be >= 2")
    }
  }

  profiles <- list()

  for (i in seq_along(plan)) {
    xs_line <- plan[i]

    # Calculate extent
    if (!missing(extent_distance)) {
      extent <- extent_distance
    } else {
      # Calculate channel width and multiply
      width <- as.numeric(sf::st_length(xs_line))
      extent <- width * extent_multiplier
    }

    # Extend the cross section line beyond banks
    extended_line <- extend_cross_section(xs_line, extent)

    # Calculate sampling distance
    if (!missing(sample_freq)) {
      sample_dist <- sample_freq
    } else {
      sample_dist <- as.numeric(sf::st_length(extended_line)) /
        max(1, sample_n - 1)
    }

    # Sample DEM along extended line
    profile_data <- sample_dem_along_line(extended_line, dem, sample_dist)

    # Create xs_profile object
    profile <- create_xs_profile(profile_data, xs_line)

    profiles[[i]] <- profile
  }

  # Update channel object with profiles
  xt_column_profile(channel) <- profiles
  channel
}

# Helper function to extend cross section line
extend_cross_section <- function(line, extent) {
  coords <- sf::st_coordinates(line)
  start_point <- coords[1, 1:2]
  end_point <- coords[nrow(coords), 1:2]

  # Calculate direction vector
  direction <- end_point - start_point
  length <- sqrt(sum(direction^2))
  unit_direction <- direction / length

  # Extend both ends
  new_start <- start_point - unit_direction * extent
  new_end <- end_point + unit_direction * extent

  # Create extended line and preserve CRS
  extended_coords <- rbind(new_start, coords[, 1:2], new_end)
  sf::st_sfc(
    sf::st_linestring(extended_coords),
    crs = sf::st_crs(line)
  )
}

# Helper function to sample DEM along line
sample_dem_along_line <- function(line, dem, sample_distance) {
  # Sample points along line
  line_length <- sf::st_length(line)
  n_points <- ceiling(as.numeric(line_length) / sample_distance) + 1L

  if (n_points < 2) {
    n_points <- 2
  }

  sample_points <- sf::st_line_sample(line, n = n_points, type = "regular")
  sample_points <- sf::st_cast(sample_points, "POINT")
  coords <- sf::st_coordinates(sample_points)

  # Guard against sampling beyond DEM extent.
  points_for_extent <- sample_points
  dem_crs <- terra::crs(dem, proj = TRUE)
  if (!is.na(sf::st_crs(sample_points)) && nzchar(dem_crs)) {
    points_for_extent <- sf::st_transform(sample_points, dem_crs)
  }
  coords_dem <- sf::st_coordinates(points_for_extent)
  outside_extent <- coords_dem[, 1] < terra::xmin(dem) |
    coords_dem[, 1] > terra::xmax(dem) |
    coords_dem[, 2] < terra::ymin(dem) |
    coords_dem[, 2] > terra::ymax(dem)
  if (any(outside_extent)) {
    stop(
      "Cross section sampling extends beyond the DEM extent. ",
      "Increase DEM coverage or reduce profile extent."
    )
  }

  # Extract elevations from DEM. Wrap the sample points in a `SpatVector` so
  # that `terra::extract()` reprojects them into the DEM's CRS when the two
  # differ. If the line has no CRS, fall back to the raw coordinates and
  # assume they are already in the DEM's CRS.
  if (!is.na(sf::st_crs(sample_points))) {
    sample_vect <- terra::vect(sample_points)
    elevations <- terra::extract(dem, sample_vect)[, 2]
  } else {
    elevations <- terra::extract(dem, coords)[, 1]
  }
  if (any(is.na(elevations))) {
    stop(
      "Missing DEM elevations encountered while sampling cross section. ",
      "Fill DEM gaps or adjust profile extent."
    )
  }

  # Calculate distances along line
  distances <- seq(0, as.numeric(line_length), length.out = n_points)

  # Return data frame
  data.frame(
    distance = distances,
    elevation = elevations,
    x = coords[, 1],
    y = coords[, 2]
  )
}

# Helper function to create xs_profile object
create_xs_profile <- function(profile_data, original_line) {
  # profile_data is sampled on the extended frame; centre distances on that
  # frame so original-bank targets at +/- width/2 map correctly.
  center_distance <- max(profile_data$distance) / 2

  # Adjust distances to be relative to center
  profile_data$relative_distance <- profile_data$distance - center_distance

  # Create coordinates matrix (NA values are guarded against upstream)
  coordinates <- as.matrix(profile_data[, c("relative_distance", "elevation")])
  if (any(!stats::complete.cases(coordinates))) {
    stop("Cannot create profile from missing coordinate or elevation values.")
  }
  if (nrow(coordinates) < 2) {
    stop("Not enough DEM samples to build profile cross section")
  }

  # Approximate bank locations at +/- half plan width, then map to nearest indices
  width <- as.numeric(sf::st_length(original_line))
  left_bank_idx <- which.min(abs(coordinates[, 1] - (-width / 2)))
  right_bank_idx <- which.min(abs(coordinates[, 1] - (width / 2)))
  if (left_bank_idx > right_bank_idx) {
    tmp <- left_bank_idx
    left_bank_idx <- right_bank_idx
    right_bank_idx <- tmp
  }

  thalweg_window <- seq.int(left_bank_idx, right_bank_idx)
  thalweg_idx <- thalweg_window[which.min(coordinates[thalweg_window, 2])]

  structure(
    list(
      coordinates = coordinates,
      banks = c(left_bank_idx, right_bank_idx),
      thalwegs = thalweg_idx,
      thalweg_elev = coordinates[thalweg_idx, 2]
    ),
    class = "xs_profile"
  )
}
