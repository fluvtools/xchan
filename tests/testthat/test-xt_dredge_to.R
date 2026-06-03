make_flat_channel <- function(
  width = 8,
  bank_elev = 10,
  crs = 3005
) {
  skip_if_not_installed("sf")
  coords <- matrix(
    c(-width / 2, bank_elev, 0, bank_elev, width / 2, bank_elev),
    ncol = 2,
    byrow = TRUE
  )
  prof <- xchan:::new_profile(coords, bankpoints = c(-width / 2, width / 2))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, width, 0), ncol = 2, byrow = TRUE)),
    crs = crs
  )
  xchan:::new_channel(seg, profile = list(prof))
}

test_that("xt_dredge_to carves a rectangular channel into a flat DEM profile", {
  ch <- make_flat_channel(width = 8, bank_elev = 10)
  out <- xt_dredge_to(ch, bathy = bathy_rectangle(depth = 2))
  prof <- out[[1]]$profile
  expect_equal(prof$thalweg_elev, 8)
  z_mid <- xchan:::coords_interpolate(prof$coordinates, 0)[2]
  expect_equal(z_mid, 8, tolerance = 1e-9)
})

test_that("xt_dredge_to fills a channel deeper than the target rectangle", {
  skip_if_not_installed("sf")
  coords <- matrix(
    c(-4, 10, 0, 5, 4, 10),
    ncol = 2,
    byrow = TRUE
  )
  prof <- xchan:::new_profile(coords, bankpoints = c(-4, 4))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 8, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(prof))
  out <- xt_dredge_to(ch, bathy = bathy_rectangle(depth = 2))
  expect_equal(out[[1]]$profile$thalweg_elev, 8)
})

test_that("xt_dredge_to applies vshape with thalweg_frac at left bank (cliff)", {
  ch <- make_flat_channel(width = 10, bank_elev = 12)
  out <- xt_dredge_to(ch, bathy = bathy_vshape(depth = 4, thalweg_frac = 0))
  prof <- out[[1]]$profile
  d_left <- xchan:::get_left_bank_coords(prof)[1]
  d_right <- xchan:::get_right_bank_coords(prof)[1]
  z_at_left <- prof$coordinates[abs(prof$coordinates[, 1] - d_left) < 1e-9, 2]
  expect_true(8 %in% z_at_left && 12 %in% z_at_left)
  expect_equal(
    xchan:::coords_interpolate(prof$coordinates, d_right)[2],
    12,
    tolerance = 1e-9
  )
})

test_that("xt_dredge_to applies vshape with thalweg_frac at right bank (cliff)", {
  ch <- make_flat_channel(width = 10, bank_elev = 12)
  out <- xt_dredge_to(ch, bathy = bathy_vshape(depth = 4, thalweg_frac = 1))
  prof <- out[[1]]$profile
  d_left <- xchan:::get_left_bank_coords(prof)[1]
  d_right <- xchan:::get_right_bank_coords(prof)[1]
  z_at_right <- prof$coordinates[abs(prof$coordinates[, 1] - d_right) < 1e-9, 2]
  expect_true(8 %in% z_at_right && 12 %in% z_at_right)
  expect_equal(
    xchan:::coords_interpolate(prof$coordinates, d_left)[2],
    12,
    tolerance = 1e-9
  )
})

test_that("xt_dredge_to applies vshape bathymetry at thalweg_frac", {
  ch <- make_flat_channel(width = 10, bank_elev = 12)
  out <- xt_dredge_to(
    ch,
    bathy = bathy_vshape(depth = 4, thalweg_frac = 0.25)
  )
  prof <- out[[1]]$profile
  d_left <- xchan:::get_left_bank_coords(prof)[1]
  d_right <- xchan:::get_right_bank_coords(prof)[1]
  d_thal <- d_left + 0.25 * (d_right - d_left)
  z_thal <- xchan:::coords_interpolate(prof$coordinates, d_thal)[2]
  expect_equal(z_thal, 8, tolerance = 1e-9)
  expect_equal(
    xchan:::coords_interpolate(prof$coordinates, d_left)[2],
    12,
    tolerance = 1e-9
  )
  expect_equal(
    xchan:::coords_interpolate(prof$coordinates, d_right)[2],
    12,
    tolerance = 1e-9
  )
})

test_that("xt_dredge_to dredges water spans and island interiors", {
  skip_if_not_installed("sf")
  coords <- matrix(
    c(
      -3, 10, -2, 9, -1, 10,
      0, 100,
      1, 10, 2, 9, 3, 10
    ),
    ncol = 2,
    byrow = TRUE
  )
  prof <- xchan:::new_profile(coords, bankpoints = c(-3, -1, 1, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(prof))
  out <- xt_dredge_to(ch, bathy = bathy_rectangle(depth = 2))
  prof2 <- out[[1]]$profile
  bd <- xchan:::get_bank_distances(prof2)
  intervals <- xchan:::bank_interval_ranges(bd)
  expect_equal(nrow(intervals), 3L)
  for (k in seq_len(nrow(intervals))) {
    lo <- intervals[k, 1]
    hi <- intervals[k, 2]
    d_mid <- (lo + hi) / 2
    z_mid <- xchan:::coords_interpolate(prof2$coordinates, d_mid)[2]
    expect_equal(z_mid, 8, tolerance = 1e-9)
  }
})

test_that("xt_dredge_to preserves outer bank elevations", {
  skip_if_not_installed("sf")
  coords <- matrix(
    c(-4, 11, 0, 10, 4, 9),
    ncol = 2,
    byrow = TRUE
  )
  prof <- xchan:::new_profile(coords, bankpoints = c(-4, 4))
  z_left <- xchan:::get_left_bank_coords(prof)[2]
  z_right <- xchan:::get_right_bank_coords(prof)[2]
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 8, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(prof))
  out <- xt_dredge_to(ch, bathy = bathy_rectangle(depth = 2))
  prof2 <- out[[1]]$profile
  expect_equal(xchan:::get_left_bank_coords(prof2)[2], z_left)
  expect_equal(xchan:::get_right_bank_coords(prof2)[2], z_right)
})

test_that("xt_dredge_to works on xsection inputs", {
  ch <- make_flat_channel()
  xs <- xt_dredge_to(ch[[1]], bathy = bathy_rectangle(depth = 1))
  expect_s3_class(xs, "xsection")
  expect_equal(xs$profile$thalweg_elev, 9)
})

test_that("xt_dredge_to requires profile cross sections", {
  ch <- xt_as_channel(c(10, 12))
  expect_error(
    xt_dredge_to(ch, bathy = bathy_rectangle(depth = 1)),
    "must have profile cross sections",
    fixed = TRUE
  )
})
