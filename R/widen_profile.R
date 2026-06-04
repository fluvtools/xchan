#' Erode a 2D cross section
#'
#' @param profile A single 2D cross section (`xs_profile`).
#' @param dw Change in width; single positive numeric.
#' @param prop_left Proportion of erosion occuring on the left bank
#' (the right bank will have `1 - prop_left` of the change in width).
#' @returns An eroded version of the input 2D cross section.
#' @details
#' Profile erosion follows three rules (applied on each bank via
#' `flip_profile()` for the opposite side):
#'
#' 1. Ground between the old and new bank positions is removed (the old bank
#' point is removed as well). 2. The **left-side channel** (topography between
#' the left bank and the leftmost thalweg) slides left by `dw`, preserving its
#' shape. The opposite bank is fixed. 3. The span between the leftmost and
#' rightmost thalwegs widens by `dw` on the eroded side; the new strip is filled
#' with a flat channel bottom at the thalweg elevation.
#'
#' A vertical bank face is placed at the new bank: its elevation is taken from
#' the pre-erosion ground surface at that distance (linear interpolation along
#' the profile). Material below the thalweg elevation in the eroded strip does
#' not count toward [xt_erosion_volume()].
#'
#' Eroding into a floodplain depression below the thalweg yields a warning and a
#' cliff down to the channel; eroding into higher ground yields a cliff that
#' rises above the channel.
#' @noRd
widen_profile <- function(
  profile,
  dw,
  prop_left
) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_number(dw, lower = 0)
  checkmate::assert_number(prop_left, lower = 0, upper = 1)
  dw_left <- prop_left * dw
  dw_right <- dw - dw_left
  bank_elev_ref <- get_bank_elevations(profile)
  profile <- widen_profile_left(profile, dw_left)
  profile <- flip_profile(profile)
  profile <- widen_profile_left(profile, dw_right)
  profile <- flip_profile(profile)
  reconcile_profile_banks(profile, bank_elevations = bank_elev_ref)
}

#' Ensure bank indices reference channel banks at each bank distance
#' @noRd
reconcile_profile_banks <- function(profile, bank_elevations = NULL) {
  bank_d <- get_bank_distances(profile)
  nodes <- profile$coordinates
  if (is.null(bank_elevations)) {
    bank_elevations <- get_bank_elevations(profile)
  }
  profile$banks <- vapply(
    seq_along(bank_d),
    function(i) {
      profile_bank_index_at_distance(nodes, bank_d[i], bank_elevations[i])
    },
    integer(1L)
  )
  profile
}

#' @noRd
thalweg_end_indices <- function(nodes, y_thalweg, tol = 1e-10) {
  at_thalweg <- abs(nodes[, 2] - y_thalweg) < tol
  if (!any(at_thalweg)) {
    return(which.min(abs(nodes[, 2] - y_thalweg)))
  }
  runs <- rle(at_thalweg)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  keep <- which(runs$values)
  unique(sort(c(starts[keep], ends[keep])))
}

#' @noRd
set_node_at_x <- function(nodes, x, y = NULL) {
  nodes <- inject_coords(nodes, x)
  at_x <- which(abs(nodes[, 1] - x) < 1e-10)
  if (length(at_x)) {
    idx <- if (is.null(y)) {
      at_x[1L]
    } else {
      at_x[which.min(abs(nodes[at_x, 2] - y))]
    }
  } else {
    idx <- which.min(abs(nodes[, 1] - x))
  }
  if (!is.null(y)) {
    nodes[idx, 2] <- y
  }
  list(nodes = nodes, idx = idx)
}

#' @noRd
widen_profile_left <- function(profile, dw) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) {
    return(profile)
  }

  nodes_orig <- profile$coordinates
  left_bank <- get_left_bank_coords(profile)
  right_bank <- get_right_bank_coords(profile)
  x_old <- left_bank[1]
  x_new <- x_old - dw
  x_extent <- min(nodes_orig[, 1])
  if (x_new < x_extent) {
    stop(
      "Cannot widen profile: requested widening exceeds cross section extent.",
      call. = FALSE
    )
  }

  thal_d_old <- get_thalweg_distances(profile)
  x_left_thal <- min(thal_d_old)
  x_right_thal <- max(thal_d_old)
  x_channel_lo <- min(x_old, x_left_thal)
  x_channel_hi <- max(x_old, x_left_thal)
  y_bed <- profile$thalweg_elev
  y_bank <- left_bank[2]
  y_cliff <- coords_interpolate(nodes_orig, x_new)[2]
  erode_into_hill <- y_cliff >= y_bank - 1e-10

  if (y_cliff < y_bed) {
    warning(
      "River has eroded into a part of the floodplain that's lower in ",
      "elevation than the thalweg. The original thalweg is still being ",
      "interpreted as the thalweg.",
      call. = FALSE
    )
  }

  nodes <- nodes_orig
  x_remove <- nodes[, 1] > x_new & nodes[, 1] < x_old
  nodes <- nodes[!x_remove, , drop = FALSE]

  left_channel <- nodes[, 1] >= x_channel_lo & nodes[, 1] <= x_channel_hi
  nodes[left_channel, 1] <- nodes[left_channel, 1] - dw

  x_left_thal_new <- x_left_thal - dw
  nodes <- set_node_at_x(nodes, x_left_thal_new, y_bed)$nodes
  if (x_right_thal > x_left_thal_new + 1e-10) {
    nodes <- set_node_at_x(nodes, x_right_thal, y_bed)$nodes
  }
  nodes <- set_node_at_x(nodes, right_bank[1], right_bank[2])$nodes

  at_x_new <- abs(nodes[, 1] - x_new) < 1e-10
  if (!any(at_x_new & abs(nodes[, 2] - y_cliff) < 1e-10)) {
    nodes <- rbind(c(x_new, y_cliff), nodes)
  }
  nodes <- nodes[order(nodes[, 1]), , drop = FALSE]

  bank_d_new <- get_bank_distances(profile)
  bank_elev <- get_bank_elevations(profile)
  left_i <- which.min(bank_d_new)
  bank_d_new[left_i] <- x_new
  bank_elev[left_i] <- y_bank
  profile$coordinates <- nodes
  profile$banks <- vapply(
    seq_along(bank_d_new),
    function(i) {
      profile_bank_index_at_distance(nodes, bank_d_new[i], bank_elev[i])
    },
    integer(1L)
  )
  profile$thalwegs <- wetted_bed_thalweg_indices(nodes, bank_d_new, y_bed)
  profile$thalweg_elev <- y_bed
  profile
}
