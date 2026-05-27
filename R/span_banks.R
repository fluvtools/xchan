#' Generate a line segment from bank to bank
#'
#' Given a point within a channel, generate a line segment that goes from
#' bank to bank, for a specified angle.
#' @param pt A point within the channel.
#' @param bankline The bankline of the channel.
#' @param angle The angle of the line segment, in radians.
#' @return A line segment spanning from bank to bank.
#' @keywords internal
#' @note Used by [xt_generate_plan()] (via `span_banks_engine()`).
span_banks <- function(pt, angle, bankline) {
  bb <- sf::st_bbox(bankline)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )
  span_banks_engine(
    pt,
    angle,
    bankline = bankline,
    maxd = maxd,
    intersect = TRUE,
    reposition = TRUE
  )
}

span_banks_engine <- function(
  pt,
  angle,
  bankline,
  maxd,
  intersect,
  reposition
) {
  if (inherits(bankline, "sf")) {
    bankline <- sf::st_geometry(bankline)
  } else if (inherits(bankline, "sfg")) {
    bankline <- sf::st_sfc(bankline, crs = sf::st_crs(bankline))
  } else if (!inherits(bankline, "sfc")) {
    stop("`bankline` must be polygon `sf`, `sfc`, or `sfg`.", call. = FALSE)
  }
  pt_coord <- sf::st_coordinates(pt)
  # Move the whole channel so that first_pt is at the origin
  bl_moved <- bankline - pt_coord
  # Construct a horizonal line going through the origin; but make sure
  # it's made up of three points -- the two ends, and the origin itself,
  # so that when it's rotated, (0, 0) is always on the line (otherwise,
  # it wouldn't be, as a result of rounding errors)
  horizontal_line <- sf::st_geometry(
    sf::st_linestring(rbind(
      c(-maxd, 0),
      #c(0, 0),
      c(maxd, 0)
    ))
  )
  # Construct the rotation matrix for a given angle (deliberately the transpose
  # of the usual rotation matrix because the matrix needs to be on the RHS of
  # the line to be rotated)
  cos_angle <- cos(angle)
  sin_angle <- sin(angle)
  rotation_matrix <- matrix(
    c(cos_angle, -sin_angle, sin_angle, cos_angle),
    ncol = 2
  )
  # Rotate the horizontal line by the given angle
  angled_line <- horizontal_line * rotation_matrix
  if (!intersect) {
    if (reposition) {
      return(angled_line + pt_coord)
    } else {
      return(angled_line)
    }
  }
  intersections <- sf::st_intersection(angled_line, bl_moved)
  if (inherits(intersections, "sfg")) {
    intersections <- sf::st_sfc(intersections)
  }
  intersections <- intersections[!sf::st_is_empty(intersections)]
  if (length(intersections) == 0L) {
    stop(
      "Line does not intersect channel polygon (empty intersection).",
      call. = FALSE
    )
  }
  if (any(sf::st_geometry_type(intersections) == "GEOMETRYCOLLECTION")) {
    intersections <- sf::st_collection_extract(
      intersections,
      type = "LINESTRING",
      warn = FALSE
    )
  }
  lines <- sf::st_cast(intersections, "MULTILINESTRING", warn = FALSE)
  lines <- sf::st_cast(lines, "LINESTRING", warn = FALSE)
  lines <- lines[!sf::st_is_empty(lines)]
  if (length(lines) == 0L) {
    stop(
      "Could not obtain line segments from intersection with channel polygon.",
      call. = FALSE
    )
  }

  origin <- sf::st_sfc(sf::st_point(c(0, 0)), crs = sf::st_crs(lines))
  on_station <- sf::st_intersects(lines, origin, sparse = FALSE)[, 1L]
  lens <- as.numeric(sf::st_length(lines))
  if (any(on_station)) {
    cand <- which(on_station)
    pick <- cand[which.min(lens[cand])]
  } else {
    dists <- as.numeric(sf::st_distance(lines, origin))
    pick <- which.min(dists)
  }
  relevant_segment <- lines[pick]
  if (reposition) {
    return(relevant_segment + pt_coord)
  } else {
    return(relevant_segment)
  }
}
