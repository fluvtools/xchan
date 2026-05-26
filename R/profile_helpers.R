#' Get bank coordinates from profile
#'
#' Extract the coordinates (distance and elevation) of bank points from a profile cross section.
#'
#' @param profile An xs_profile object
#' @returns A matrix with bank coordinates (distance, elevation)
#' @noRd
get_bank_coords <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[profile$banks, , drop = FALSE]
}

#' Get thalweg coordinates from profile
#'
#' Extract the coordinates (distance and elevation) of thalweg points from a profile cross section.
#'
#' @param profile An xs_profile object
#' @returns A matrix with thalweg coordinates (distance, elevation)
#' @noRd
get_thalweg_coords <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[profile$thalwegs, , drop = FALSE]
}

#' Row index of a bank vertex at a distance (cliff-aware)
#'
#' When several vertices share a bank distance, pick the channel bank rather
#' than the opposite end of a vertical cliff face.
#'
#' @noRd
profile_bank_index_at_distance <- function(
  nodes,
  xd,
  y_bank,
  tol = 1e-10
) {
  at <- abs(nodes[, 1] - xd) < tol
  if (!any(at)) {
    return(which.min(abs(nodes[, 1] - xd)))
  }
  at_idx <- which(at)
  if (length(at_idx) == 1L) {
    return(at_idx)
  }
  ys <- nodes[at_idx, 2]
  if (min(ys) < y_bank - tol) {
    return(at_idx[which.max(ys)])
  }
  at_idx[which.min(abs(ys - y_bank))]
}

#' Get bank distances from profile
#'
#' Extract the distance values of bank points from a profile cross section.
#'
#' @param profile An xs_profile object
#' @returns A numeric vector of bank distances
#' @noRd
get_bank_distances <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[profile$banks, 1]
}

#' Recenter profile distances on the outer-bank midpoint
#'
#' @noRd
recenter_profile_distances <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  shift <- mean(range(get_bank_distances(profile)))
  if (abs(shift) < 1e-10) {
    return(profile)
  }
  profile$coordinates[, 1] <- profile$coordinates[, 1] - shift
  profile
}

#' Water-interval distance pairs from bank contacts along a transect
#'
#' @param bank_d Numeric bank distances (even length).
#' @returns Matrix with two columns (`lo`, `hi`), one row per water interval.
#' @noRd
water_interval_ranges <- function(bank_d) {
  checkmate::assert_numeric(bank_d, any.missing = FALSE, min.len = 2L)
  if (length(bank_d) %% 2L != 0L) {
    stop(
      "Expected an even number of bank points along the transect (got ",
      length(bank_d),
      ").",
      call. = FALSE
    )
  }
  matrix(bank_d, ncol = 2L, byrow = TRUE)
}

#' Row indices of bank contacts along the profile polyline
#'
#' For duplicate `x` values at a vertical cliff, the wetted side is the last row
#' at a left-bank distance and the first row at a right-bank distance.
#'
#' @noRd
bank_contact_row_indices <- function(coords, bank_d, tol = 1e-10) {
  checkmate::assert_matrix(coords, mode = "numeric", ncols = 2L)
  checkmate::assert_numeric(bank_d, any.missing = FALSE, min.len = 2L)
  vapply(
    seq_along(bank_d),
    function(i) {
      at <- which(abs(coords[, 1] - bank_d[i]) < tol)
      if (!length(at)) {
        return(which.min(abs(coords[, 1] - bank_d[i])))
      }
      if (i %% 2L == 1L) {
        max(at)
      } else {
        min(at)
      }
    },
    integer(1L)
  )
}

#' Row mask for wetted profile vertices
#'
#' @noRd
wetted_vertex_mask <- function(coords, bank_d, tol = 1e-10) {
  checkmate::assert_matrix(coords, mode = "numeric", ncols = 2L)
  mask <- rep(FALSE, nrow(coords))
  bank_rows <- bank_contact_row_indices(coords, bank_d, tol = tol)
  intervals <- matrix(bank_rows, ncol = 2L, byrow = TRUE)
  for (k in seq_len(nrow(intervals))) {
    lo <- intervals[k, 1]
    hi <- intervals[k, 2]
    mask[seq.int(lo, hi)] <- TRUE
  }
  mask
}

#' Thalweg row indices from wetted intervals only
#'
#' @noRd
thalweg_indices_from_banks <- function(coords, bank_d, tol = 1e-10) {
  if (length(bank_d) <= 2L) {
    return(thalweg_end_indices(coords, min(coords[, 2]), tol = tol))
  }
  wetted <- wetted_vertex_mask(coords, bank_d, tol = tol)
  if (!any(wetted)) {
    stop("No wetted profile vertices found between bank pairs.", call. = FALSE)
  }
  y_min <- min(coords[wetted, 2])
  at_thalweg <- wetted & abs(coords[, 2] - y_min) < tol
  runs <- rle(at_thalweg)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  keep <- which(runs$values)
  unique(sort(c(starts[keep], ends[keep])))
}

#' Thalweg endpoints along flat bed within wetted intervals
#'
#' @noRd
wetted_bed_thalweg_indices <- function(coords, bank_d, y_bed, tol = 1e-10) {
  wetted <- wetted_vertex_mask(coords, bank_d, tol = tol)
  at_bed <- wetted & abs(coords[, 2] - y_bed) < tol
  if (!any(at_bed)) {
    return(thalweg_indices_from_banks(coords, bank_d, tol = tol))
  }
  runs <- rle(at_bed)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  keep <- which(runs$values)
  unique(sort(c(starts[keep], ends[keep])))
}

#' Profile coordinate matrices for sf export (one list element per water interval)
#'
#' @noRd
profile_coord_parts_for_extent <- function(
  profile,
  extent = c("banks", "full", "wetted")
) {
  extent <- match.arg(extent)
  checkmate::assert_class(profile, "xs_profile")
  coords <- profile$coordinates

  if (extent == "banks") {
    bd <- range(get_bank_distances(profile))
    coords <- coords[
      coords[, 1] >= bd[1] & coords[, 1] <= bd[2],
      ,
      drop = FALSE
    ]
    return(list(coords))
  }
  if (extent == "full") {
    return(list(coords))
  }

  intervals <- matrix(
    bank_contact_row_indices(coords, get_bank_distances(profile)),
    ncol = 2L,
    byrow = TRUE
  )
  lapply(seq_len(nrow(intervals)), function(k) {
    lo <- intervals[k, 1]
    hi <- intervals[k, 2]
    coords[seq.int(lo, hi), , drop = FALSE]
  })
}

#' Combine linestrings as one `LINESTRING` or `MULTILINESTRING`
#'
#' @noRd
as_line_or_multilinestring <- function(lines) {
  if (length(lines) == 1L) {
    return(lines[[1]])
  }
  sf::st_multilinestring(lapply(lines, sf::st_coordinates))
}

#' Get thalweg distances from profile
#'
#' Extract the distance values of thalweg points from a profile cross section.
#'
#' @param profile An xs_profile object
#' @returns A numeric vector of thalweg distances
#' @noRd
get_thalweg_distances <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[profile$thalwegs, 1]
}

#' Get bank elevations from profile
#'
#' Extract the elevation values of bank points from a profile cross section.
#'
#' @param profile An xs_profile object
#' @returns A numeric vector of bank elevations
#' @noRd
get_bank_elevations <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[profile$banks, 2]
}

#' Get thalweg elevations from profile
#'
#' Extract the elevation values of thalweg points from a profile cross section.
#'
#' @param profile An xs_profile object
#' @returns A numeric vector of thalweg elevations
#' @noRd
get_thalweg_elevations <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[profile$thalwegs, 2]
}

#' Get leftmost bank index
#'
#' Get the index of the leftmost bank point.
#'
#' @param profile An xs_profile object
#' @returns Integer index of leftmost bank
#' @noRd
get_left_bank_index <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  bank_distances <- get_bank_distances(profile)
  profile$banks[which.min(bank_distances)]
}

#' Get rightmost bank index
#'
#' Get the index of the rightmost bank point.
#'
#' @param profile An xs_profile object
#' @returns Integer index of rightmost bank
#' @noRd
get_right_bank_index <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  bank_distances <- get_bank_distances(profile)
  profile$banks[which.max(bank_distances)]
}

#' Get leftmost bank coordinates
#'
#' Get the coordinates of the leftmost bank point.
#'
#' @param profile An xs_profile object
#' @returns Numeric vector of length 2 (distance, elevation)
#' @noRd
get_left_bank_coords <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[get_left_bank_index(profile), ]
}

#' Get rightmost bank coordinates
#'
#' Get the coordinates of the rightmost bank point.
#'
#' @param profile An xs_profile object
#' @returns Numeric vector of length 2 (distance, elevation)
#' @noRd
get_right_bank_coords <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[get_right_bank_index(profile), ]
}

#' Snap outer bank horizontal positions
#'
#' Sets left and right bank distances without changing elevations so
#' [xt_width()] returns `right_x - left_x`.
#'
#' @param profile An `xs_profile` object.
#' @param left_x,right_x Target bank distances along the profile.
#' @noRd
snap_profile_bank_positions <- function(profile, left_x, right_x) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_number(left_x, finite = TRUE)
  checkmate::assert_number(right_x, finite = TRUE)
  if (left_x >= right_x) {
    stop("Left bank distance must be less than right bank distance.", call. = FALSE)
  }
  coords <- profile$coordinates
  bank_d <- get_bank_distances(profile)
  bank_elev <- get_bank_elevations(profile)
  left_i <- which.min(bank_d)
  right_i <- which.max(bank_d)
  lb <- profile$banks[left_i]
  rb <- profile$banks[right_i]
  left_at <- abs(coords[, 1] - coords[lb, 1]) < 1e-10
  right_at <- abs(coords[, 1] - coords[rb, 1]) < 1e-10
  coords[left_at, 1] <- left_x
  coords[right_at, 1] <- right_x
  coords <- coords[order(coords[, 1]), , drop = FALSE]
  profile$coordinates <- coords
  bank_d[left_i] <- left_x
  bank_d[right_i] <- right_x
  profile$banks <- vapply(
    seq_along(bank_d),
    function(i) {
      profile_bank_index_at_distance(coords, bank_d[i], bank_elev[i])
    },
    integer(1L)
  )
  profile$thalwegs <- wetted_bed_thalweg_indices(coords, bank_d, profile$thalweg_elev)
  profile$thalweg_elev <- min(coords[profile$thalwegs, 2])
  profile
}

#' Get thalweg index with minimum elevation
#'
#' Get the index of the thalweg point with the lowest elevation.
#'
#' @param profile An xs_profile object
#' @returns Integer index of thalweg with minimum elevation
#' @noRd
get_min_thalweg_index <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  thalweg_elevations <- get_thalweg_elevations(profile)
  profile$thalwegs[which.min(thalweg_elevations)]
}

#' Get thalweg coordinates with minimum elevation
#'
#' Get the coordinates of the thalweg point with the lowest elevation.
#'
#' @param profile An xs_profile object
#' @returns Numeric vector of length 2 (distance, elevation)
#' @noRd
get_min_thalweg_coords <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  profile$coordinates[get_min_thalweg_index(profile), ]
}

#' Label a channel cross section for error messages
#'
#' @param channel An `xchan` object.
#' @param i Integer index into `channel`.
#' @noRd
section_label_at <- function(channel, i) {
  sid <- attr(channel, "section_i", exact = TRUE)
  if (!is.null(sid) && length(sid) >= i) {
    id <- xchan_print_section_id_value(sid[[i]])
    paste0("cross section ", i, " (id = ", id, ")")
  } else {
    paste0("cross section ", i)
  }
}

#' @param failures List of `list(label = , message = )` from per-section errors.
#' @noRd
stop_erosion_section_errors <- function(failures) {
  if (!length(failures)) {
    return(invisible())
  }
  lines <- vapply(
    failures,
    function(f) paste0(f$label, ": ", f$message),
    character(1)
  )
  stop(paste(lines, collapse = "\n"), call. = FALSE)
}
