# Standalone sandbox helpers for artificial islands in Fraser bankline.
# This script is intentionally outside package API/tests/docs.

.island_station <- function(plan_sfc, fraction) {
  i <- as.integer(round(fraction * (length(plan_sfc) - 1))) + 1L
  i <- max(1L, min(length(plan_sfc), i))
  xs <- plan_sfc[i]

  xy <- sf::st_coordinates(xs)
  left <- xy[1, 1:2]
  right <- xy[nrow(xy), 1:2]
  width <- as.numeric(sf::st_length(xs))
  lr_unit <- (left - right) / sqrt(sum((left - right)^2))
  ds_unit <- c(-lr_unit[2], lr_unit[1])
  centerline_xy <- 0.5 * (left + right)

  list(
    xs = xs,
    width = width,
    lr_unit = lr_unit,
    ds_unit = ds_unit,
    centerline_xy = centerline_xy
  )
}

.build_capsule_island <- function(
  station_info,
  width_fraction,
  length_fraction,
  offset,
  crs,
  n_quad_segs
) {
  width <- station_info$width
  half_width <- 0.5 * width_fraction * width
  half_length <- 0.5 * length_fraction * width
  center <- station_info$centerline_xy +
    offset * (0.5 * width - half_width) * station_info$lr_unit

  p0 <- center - half_length * station_info$ds_unit
  p1 <- center + half_length * station_info$ds_unit
  centerline_seg <- sf::st_sfc(sf::st_linestring(rbind(p0, p1)), crs = crs)
  sf::st_buffer(
    centerline_seg,
    dist = half_width,
    nQuadSegs = as.integer(n_quad_segs)
  )
}

inject_demo_islands <- function(
  bankline = NULL,
  fractions = c(0.56, 0.56),
  width_fractions = c(0.14, 0.14),
  length_fractions = c(0.85, 0.85),
  offsets = c(-0.35, 0.35),
  n_sections = 121,
  n_quad_segs = 12
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("xchan", quietly = TRUE)) {
    stop("Package 'xchan' is required.")
  }
  if (is.null(bankline)) {
    bankline <- xchan::demo_bankline
  }
  if (!inherits(bankline, "sfc")) {
    stop("`bankline` must be an sfc polygon geometry.")
  }

  k <- length(fractions)
  if (k < 1L) {
    stop("Provide at least one island.")
  }
  if (
    length(width_fractions) != k ||
      length(length_fractions) != k ||
      length(offsets) != k
  ) {
    stop(
      "`fractions`, `width_fractions`, `length_fractions`, and `offsets` must have equal length."
    )
  }
  if (any(fractions < 0 | fractions > 1)) {
    stop("All `fractions` must be in [0, 1].")
  }
  if (any(width_fractions <= 0 | width_fractions >= 1)) {
    stop("All `width_fractions` must be in (0, 1).")
  }
  if (any(length_fractions <= 0)) {
    stop("All `length_fractions` must be > 0.")
  }

  ch <- xchan::xt_generate_plan(bankline, n = n_sections)
  crs <- sf::st_crs(bankline)
  islands <- vector("list", k)

  for (j in seq_len(k)) {
    s <- .island_station(xchan::xt_as_sfc(ch, what = "plan"), fractions[j])
    island_try <- .build_capsule_island(
      station_info = s,
      width_fraction = width_fractions[j],
      length_fraction = length_fractions[j],
      offset = offsets[j],
      crs = crs,
      n_quad_segs = n_quad_segs
    )

    # Keep shrinking until each island is fully inside the bankline.
    scale <- 1
    ok <- any(sf::st_within(island_try, bankline, sparse = FALSE))
    while (!ok && scale > 0.1) {
      scale <- scale * 0.8
      island_try <- .build_capsule_island(
        station_info = s,
        width_fraction = width_fractions[j] * scale,
        length_fraction = length_fractions[j] * scale,
        offset = offsets[j],
        crs = crs,
        n_quad_segs = n_quad_segs
      )
      ok <- any(sf::st_within(island_try, bankline, sparse = FALSE))
    }
    if (!ok) {
      stop(
        "Could not place island ",
        j,
        " fully within bankline; try smaller width/length fractions or different offsets."
      )
    }
    islands[[j]] <- island_try[[1]]
  }

  islands_sfc <- sf::st_sfc(islands, crs = crs)
  merged_islands <- sf::st_union(islands_sfc)
  bankline_with_islands <- sf::st_difference(bankline, merged_islands)

  # Useful diagnostic: do any sampled cross sections intersect all islands?
  plan_sfc <- xchan::xt_as_sfc(ch, what = "plan")
  ix <- sf::st_intersects(plan_sfc, islands_sfc, sparse = FALSE)
  overlap_sections <- which(rowSums(ix) == ncol(ix))

  list(
    bankline_with_islands = bankline_with_islands,
    islands = islands_sfc,
    sampled_plan = plan_sfc,
    overlap_sections = overlap_sections
  )
}

inject_demo_island <- function(
  bankline = NULL,
  fraction = 0.56,
  width_fraction = 0.16,
  length_fraction = 0.9,
  offset = 0,
  n_sections = 121,
  n_quad_segs = 12
) {
  out <- inject_demo_islands(
    bankline = bankline,
    fractions = fraction,
    width_fractions = width_fraction,
    length_fractions = length_fraction,
    offsets = offset,
    n_sections = n_sections,
    n_quad_segs = n_quad_segs
  )
  out$island <- out$islands[1]
  out$islands <- NULL
  out
}

# ---- quick play examples ----
# library(xchan)
# source("excluded/fraser_island_sandbox.R")
#
# Single long/narrow island:
one <- inject_demo_island(
  fraction = 0.57,
  width_fraction = 0.12,
  length_fraction = 1.1,
  offset = 0.1
)

# Two islands configured to overlap the same cross sections:
two <- inject_demo_islands(
  fractions = c(0.57, 0.57),
  width_fractions = c(0.12, 0.12),
  length_fractions = c(1.0, 1.0),
  offsets = c(-0.35, 0.35)
)
length(two$overlap_sections) # should usually be > 0

plot(demo_bankline, col = "grey90", border = "grey50")
plot(two$bankline_with_islands, add = TRUE, col = "#9ecae1", border = "#3182bd")
plot(two$islands, add = TRUE, col = "#fdd0a2", border = "#e6550d")
plot(
  two$sampled_plan[two$overlap_sections],
  add = TRUE,
  col = "#6a51a3",
  lwd = 2
)

foo <- xt_generate_plan(two$bankline_with_islands, n = 100, progress = TRUE)
