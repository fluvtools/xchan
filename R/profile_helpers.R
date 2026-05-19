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
