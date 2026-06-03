#' Remove duplicate coordinate rows
#' @noRd
dedupe_coordinate_rows <- function(coords, tol = 1e-10) {
  if (!nrow(coords)) {
    return(coords)
  }
  out <- coords
  i <- 2L
  while (i <= nrow(out)) {
    if (
      abs(out[i, 1] - out[i - 1L, 1]) < tol &&
        abs(out[i, 2] - out[i - 1L, 2]) < tol
    ) {
      out <- out[-i, , drop = FALSE]
    } else {
      i <- i + 1L
    }
  }
  out
}

#' Build profile nodes for one wetted interval under target bathymetry
#' @noRd
build_dredged_wetted_nodes <- function(
  d_left,
  z_left,
  d_right,
  z_right,
  bed_elev_fn,
  interior_d = NULL,
  tol = 1e-10
) {
  if (d_left >= d_right) {
    stop(
      "Left bank distance must be less than right bank distance.",
      call. = FALSE
    )
  }
  z_bed_left <- bed_elev_fn(d_left)
  z_bed_right <- bed_elev_fn(d_right)
  if (is.null(interior_d)) {
    interior_d <- numeric(0)
  }
  interior_d <- interior_d[
    interior_d > d_left + tol & interior_d < d_right - tol
  ]
  interior_d <- unique(sort(interior_d))

  pts <- list(c(d_left, z_left))
  if (z_left > z_bed_left + tol) {
    pts <- c(pts, list(c(d_left, z_bed_left)))
  }
  for (d in interior_d) {
    pts <- c(pts, list(c(d, bed_elev_fn(d))))
  }
  if (abs(z_bed_right - z_bed_left) > tol || !length(interior_d)) {
    if (abs(d_right - d_left) > tol) {
      pts <- c(pts, list(c(d_right, z_bed_right)))
    }
  }
  if (z_right > z_bed_right + tol) {
    last <- pts[[length(pts)]]
    if (abs(last[1] - d_right) > tol || abs(last[2] - z_bed_right) > tol) {
      pts <- c(pts, list(c(d_right, z_bed_right)))
    }
  }
  last <- pts[[length(pts)]]
  if (abs(last[1] - d_right) > tol || abs(last[2] - z_right) > tol) {
    pts <- c(pts, list(c(d_right, z_right)))
  }
  do.call(rbind, pts)
}

#' Bed specification for one wetted interval
#' @noRd
bed_spec_for_interval <- function(bathy, wse, d_left, d_right) {
  if (bathy$shape == "rectangle") {
    z_bed <- wse - bathy$depth
    return(list(
      bed_elev_fn = function(d) z_bed,
      interior_d = NULL
    ))
  }
  if (bathy$shape == "vshape") {
    d_thal <- d_left + bathy$thalweg_frac * (d_right - d_left)
    depth <- bathy$depth
    bed_elev_fn <- function(d) {
      if (d <= d_thal) {
        if (abs(d_thal - d_left) < 1e-10) {
          return(wse - depth)
        }
        t <- (d - d_left) / (d_thal - d_left)
        wse - t * depth
      } else {
        if (abs(d_right - d_thal) < 1e-10) {
          return(wse - depth)
        }
        t <- (d - d_thal) / (d_right - d_thal)
        (wse - depth) + t * depth
      }
    }
    return(list(
      bed_elev_fn = bed_elev_fn,
      interior_d = d_thal
    ))
  }
  stop("Unsupported bathymetry shape.", call. = FALSE)
}

#' Distance spans between consecutive bank contacts
#' @noRd
bank_interval_ranges <- function(bank_d) {
  checkmate::assert_numeric(bank_d, any.missing = FALSE, min.len = 2L)
  n <- length(bank_d)
  cbind(bank_d[-n], bank_d[-1])
}

#' Replace profile beds with target bathymetry
#'
#' Every span between consecutive bank contacts is dredged independently,
#' including mid-channel island interiors.
#' @noRd
dredge_profile <- function(profile, bathy, wse) {
  checkmate::assert_class(profile, "xs_profile")
  tol <- 1e-10

  bank_d <- get_bank_distances(profile)
  bank_elev <- get_bank_elevations(profile)
  if (length(bank_d) %% 2L != 0L) {
    stop(
      "Expected an even number of bank points along the transect (got ",
      length(bank_d),
      ").",
      call. = FALSE
    )
  }

  intervals <- bank_interval_ranges(bank_d)
  n_intervals <- nrow(intervals)
  nodes <- profile$coordinates
  d_outer_lo <- min(bank_d)
  d_outer_hi <- max(bank_d)
  kept_nodes <- nodes[
    nodes[, 1] < d_outer_lo - tol | nodes[, 1] > d_outer_hi + tol,
    ,
    drop = FALSE
  ]

  dredged <- lapply(seq_len(n_intervals), function(k) {
    d_left <- intervals[k, 1]
    d_right <- intervals[k, 2]
    z_left <- bank_elev[k]
    z_right <- bank_elev[k + 1L]
    spec <- bed_spec_for_interval(bathy, wse, d_left, d_right)
    build_dredged_wetted_nodes(
      d_left,
      z_left,
      d_right,
      z_right,
      spec$bed_elev_fn,
      interior_d = spec$interior_d,
      tol = tol
    )
  })

  new_nodes <- kept_nodes[0, , drop = FALSE]
  for (k in seq_len(n_intervals)) {
    seg <- dredged[[k]]
    if (!nrow(new_nodes)) {
      new_nodes <- seg
      next
    }
    last <- new_nodes[nrow(new_nodes), , drop = FALSE]
    first <- seg[1, , drop = FALSE]
    if (all(abs(last - first) < tol)) {
      seg <- seg[-1, , drop = FALSE]
    }
    if (nrow(seg)) {
      new_nodes <- rbind(new_nodes, seg)
    }
  }

  if (nrow(kept_nodes)) {
    left_kept <- kept_nodes[kept_nodes[, 1] < d_outer_lo + tol, , drop = FALSE]
    right_kept <- kept_nodes[kept_nodes[, 1] > d_outer_hi - tol, , drop = FALSE]
    if (nrow(left_kept)) {
      left_kept <- left_kept[order(left_kept[, 1]), , drop = FALSE]
      last <- left_kept[nrow(left_kept), , drop = FALSE]
      first <- new_nodes[1, , drop = FALSE]
      if (all(abs(last - first) < tol)) {
        new_nodes <- new_nodes[-1, , drop = FALSE]
      }
      new_nodes <- rbind(left_kept, new_nodes)
    }
    if (nrow(right_kept)) {
      right_kept <- right_kept[order(right_kept[, 1]), , drop = FALSE]
      last <- new_nodes[nrow(new_nodes), , drop = FALSE]
      first <- right_kept[1, , drop = FALSE]
      if (all(abs(last - first) < tol)) {
        right_kept <- right_kept[-1, , drop = FALSE]
      }
      if (nrow(right_kept)) {
        new_nodes <- rbind(new_nodes, right_kept)
      }
    }
  }

  new_nodes <- dedupe_coordinate_rows(new_nodes, tol = tol)

  profile$coordinates <- new_nodes
  profile$banks <- vapply(
    seq_along(bank_d),
    function(i) {
      profile_bank_index_at_distance(new_nodes, bank_d[i], bank_elev[i])
    },
    integer(1L)
  )
  y_bed <- min(vapply(dredged, function(m) min(m[, 2]), numeric(1)))
  profile$thalwegs <- wetted_bed_thalweg_indices(
    new_nodes,
    bank_d,
    y_bed,
    tol = tol
  )
  profile$thalweg_elev <- min(new_nodes[profile$thalwegs, 2])
  profile
}

#' Apply target bathymetry to all profile cross sections in a channel
#' @noRd
apply_bathymetry <- function(channel, bathy) {
  profiles <- channel_profile(channel)
  wse <- resolve_bathymetry_wse(bathy$wse, channel)
  new_profiles <- Map(
    function(prof, ws) {
      dredge_profile(prof, bathy, ws)
    },
    profiles,
    wse
  )
  set_channel_profile(channel, new_profiles)
}

#' Resolve water-surface elevation per cross section
#' @noRd
resolve_bathymetry_wse <- function(wse, channel) {
  if (inherits(wse, "xchan_elevation")) {
    return(wse(channel))
  }
  stop(
    "`wse` must be an elevation specification (for example ",
    "`elevation_bank()`).",
    call. = FALSE
  )
}
