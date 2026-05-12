#' Sample DEM to generate profile cross sections
#'
#' Generate profile cross sections for a channel object using a digital
#' elevation model (DEM). This function samples the DEM along the planimetric
#' cross sections and creates xs_profile objects for each cross section.
#'
#' @param channel [`xchan`] with planimetric cross sections.
#' @param dem Digital elevation model (raster or terra object).
#' @param ... Must be empty.
#' @param extent_distance Distance to extend beyond banks on each side. Use
#'   `Inf` (the default) to extend along the cross section until the DEM
#'   bounding box is reached in each direction. A finite plain numeric is
#'   interpreted in the channel's CRS length unit; a [units::units()] length
#'   object is converted automatically. If larger than the distance from each
#'   bank to the DEM edge along the cross-section line, the extension is
#'   shortened silently on that side. Mutually exclusive with passing a
#'   non-`NULL` `extent_multiplier`.
#' @param extent_multiplier Multiplier of channel width to extend beyond banks;
#'   positive numeric value. Default `NULL` uses `extent_distance` instead.
#'   Mutually exclusive with supplying an explicit `extent_distance` argument.
#' @param sample_freq Distance between DEM sampling points; positive value.
#'   Same units treatment as `extent_distance`. Mutually exclusive with
#'   `sample_n`.
#' @param sample_n number to sample; positive integer value greater than 1.
#'   Mutually exclusive with `sample_freq`.
#' @param progress If `TRUE`, show a text progress bar while processing cross
#'   sections (same behaviour as [xt_generate_plan()]).
#' @returns Updated [`xchan`] with profile geometry attached to each cross section.
#' @details This function extends the planimetric cross sections beyond the
#'   banks to create a "frame" for erosion analysis. The extent can be specified
#'   either as a fixed distance or as a multiplier of the channel width.
#'   Similarly, the sampling distance can be specified either as a fixed
#'   distance or as a multiplier of the channel width.
#'
#'   Extension beyond banks is clipped to the DEM bounding box (in the DEM's
#'   CRS, projected to the cross-section CRS when needed), so sampling stays
#'   inside raster coverage if the bank-to-bank segment lies inside the DEM.
#'
#'   Elevations are taken with bilinear interpolation between cell centres (see
#'   [terra::extract()] with `method = "bilinear"`), which smooths grid-oriented
#'   steps relative to nearest-neighbour extraction but still respects the DEM
#'   cell size: expect subtle terrace-like breaks where slope crosses grid
#'   boundaries if resolution is coarse.
#'
#'   Sample points that fall in nodata cells (`NA`) at the **ends** of the
#'   transect are dropped until all remaining elevations are finite; interior
#'   gaps still raise an error. Any sample outside the raster bounding box
#'   raises an error.
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
  extent_distance = Inf,
  extent_multiplier = NULL,
  sample_freq,
  sample_n,
  progress = FALSE
) {
  rlang::check_dots_empty()
  if (!rlang::is_bool(progress)) {
    stop("`progress` must be TRUE or FALSE.")
  }

  checkmate::assert_class(channel, "xchan")

  plan <- channel_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  use_multiplier <- !is.null(extent_multiplier)
  if (use_multiplier && !missing(extent_distance)) {
    stop(
      "Pass only one of `extent_distance` or `extent_multiplier`.",
      call. = FALSE
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
  if (use_multiplier) {
    checkmate::assert_number(extent_multiplier, lower = 0, finite = TRUE)
  } else {
    if (is.infinite(extent_distance)) {
      if (!identical(extent_distance, Inf)) {
        stop("`extent_distance` must be non-negative or `Inf`.", call. = FALSE)
      }
    } else {
      extent_distance <- to_numeric_length(
        extent_distance,
        unit,
        arg = "extent_distance"
      )
      checkmate::assert_number(extent_distance, lower = 0, finite = TRUE)
    }
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

  n_plan <- length(plan)
  pb <- NULL
  if (progress && n_plan > 0L) {
    pb <- utils::txtProgressBar(min = 0, max = n_plan, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_along(plan)) {
    xs_line <- plan[i]

    bb <- dem_bbox_for_line(dem, sf::st_crs(xs_line))
    lims <- cross_section_bank_ray_limits(xs_line, bb)

    if (use_multiplier) {
      width <- as.numeric(sf::st_length(xs_line))
      extent_req <- width * extent_multiplier
    } else {
      extent_req <- extent_distance
    }

    el <- if (is.infinite(extent_req)) {
      lims$left
    } else {
      min(extent_req, lims$left)
    }
    er <- if (is.infinite(extent_req)) {
      lims$right
    } else {
      min(extent_req, lims$right)
    }

    # Extend the cross section line beyond banks (clipped to DEM bounds)
    extended_line <- extend_cross_section(xs_line, el, er)

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

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  # Update channel object with profiles
  channel <- set_channel_profile(channel, profiles)
  channel
}

# Helper function to extend cross section line (possibly asymmetric)
extend_cross_section <- function(line, extent_left, extent_right) {
  coords <- sf::st_coordinates(line)
  start_point <- coords[1, 1:2]
  end_point <- coords[nrow(coords), 1:2]

  # Calculate direction vector
  direction <- end_point - start_point
  seg_len <- sqrt(sum(direction^2))
  if (seg_len < .Machine$double.eps) {
    stop("Degenerate planimetric cross section (zero length).", call. = FALSE)
  }
  unit_direction <- direction / seg_len

  # Extend both ends
  new_start <- start_point - unit_direction * extent_left
  new_end <- end_point + unit_direction * extent_right

  # Create extended line and preserve CRS
  extended_coords <- rbind(new_start, coords[, 1:2], new_end)
  sf::st_sfc(
    sf::st_linestring(extended_coords),
    crs = sf::st_crs(line)
  )
}

#' @noRd
dem_bbox_for_line <- function(dem, line_crs) {
  ext <- terra::ext(dem)
  xmin <- unname(ext[1])
  xmax <- unname(ext[2])
  ymin <- unname(ext[3])
  ymax <- unname(ext[4])
  dem_crs <- terra::crs(dem)
  if (is.na(line_crs)) {
    return(list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
  }
  bb <- sf::st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    crs = dem_crs
  )
  poly <- sf::st_as_sfc(bb)
  poly <- sf::st_transform(poly, line_crs)
  bb2 <- sf::st_bbox(poly)
  list(
    xmin = unname(bb2$xmin),
    xmax = unname(bb2$xmax),
    ymin = unname(bb2$ymin),
    ymax = unname(bb2$ymax)
  )
}

#' @noRd
max_ray_distance_in_rect <- function(origin, dir, xmin, xmax, ymin, ymax) {
  inside <- function(t) {
    p <- origin + dir * t
    p[1] >= xmin && p[1] <= xmax && p[2] >= ymin && p[2] <= ymax
  }
  if (!inside(0)) {
    return(0)
  }
  span <- max(xmax - xmin, ymax - ymin)
  hi <- span
  step <- 0L
  while (inside(hi) && step < 80L) {
    hi <- hi * 2
    step <- step + 1L
  }
  if (inside(hi)) {
    return(hi)
  }
  lo <- 0
  for (i in seq_len(80L)) {
    mid <- (lo + hi) / 2
    if (inside(mid)) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  lo
}

#' @noRd
cross_section_bank_ray_limits <- function(line, bb) {
  coords <- sf::st_coordinates(line)
  start_point <- coords[1, 1:2]
  end_point <- coords[nrow(coords), 1:2]
  direction <- end_point - start_point
  seg_len <- sqrt(sum(direction^2))
  if (seg_len < .Machine$double.eps) {
    return(list(left = 0, right = 0))
  }
  unit_direction <- direction / seg_len
  list(
    left = max_ray_distance_in_rect(
      start_point,
      -unit_direction,
      bb$xmin,
      bb$xmax,
      bb$ymin,
      bb$ymax
    ),
    right = max_ray_distance_in_rect(
      end_point,
      unit_direction,
      bb$xmin,
      bb$xmax,
      bb$ymin,
      bb$ymax
    )
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
    extr <- terra::extract(dem, sample_vect, method = "bilinear")
    lyr <- names(dem)[1L]
    elevations <- extr[[lyr]]
  } else {
    pts_raw <- terra::vect(coords, crs = terra::crs(dem))
    extr <- terra::extract(dem, pts_raw, method = "bilinear")
    lyr <- names(dem)[1L]
    elevations <- extr[[lyr]]
  }

  ok <- !is.na(elevations)
  if (!any(ok)) {
    stop(
      "Missing DEM elevations encountered while sampling cross section. ",
      "Fill DEM gaps or adjust profile extent.",
      call. = FALSE
    )
  }

  i1 <- min(which(ok))
  i2 <- max(which(ok))
  coords <- coords[i1:i2, , drop = FALSE]
  elevations <- elevations[i1:i2]

  if (any(is.na(elevations))) {
    stop(
      "Missing DEM elevations along the interior of the cross section ",
      "(nodata inside the transect). Fill DEM gaps or reduce extent.",
      call. = FALSE
    )
  }

  if (nrow(coords) < 2L) {
    stop(
      "Not enough valid DEM samples along the cross section after trimming ",
      "nodata at the transect ends.",
      call. = FALSE
    )
  }

  seg_lens <- sqrt(diff(coords[, 1])^2 + diff(coords[, 2])^2)
  distances <- c(0, cumsum(seg_lens))

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

  # Snap bank distances to the plan width so discrete sampling does not leave
  # planimetric width and profile width disagreeing by a fraction of a sample.
  coordinates[left_bank_idx, 1] <- -width / 2
  coordinates[right_bank_idx, 1] <- width / 2

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
