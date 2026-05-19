test_that("xt_generate_profile errors when cross section lies outside DEM extent", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  dem <- terra::rast(
    xmin = -2,
    xmax = 2,
    ymin = -2,
    ymax = 2,
    ncols = 20,
    nrows = 20,
    crs = "EPSG:3857"
  )
  terra::values(dem) <- 100
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(5, 0, 7, 0), ncol = 2, byrow = TRUE)),
    crs = "EPSG:3857"
  )
  channel <- xchan:::new_channel(seg)

  expect_error(
    xt_generate_profile(channel, dem, extent_distance = 0, sample_n = 10),
    "beyond the DEM extent"
  )
})

test_that("xt_generate_profile clips requested extent to DEM bounds", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  dem <- terra::rast(
    xmin = -2,
    xmax = 2,
    ymin = -2,
    ymax = 2,
    ncols = 20,
    nrows = 20,
    crs = "EPSG:3857"
  )
  terra::values(dem) <- 100
  channel <- xt_as_channel(2, crs = 3857)

  expect_no_error(
    xt_generate_profile(
      channel,
      dem,
      extent_distance = 500,
      sample_n = 10
    )
  )

  expect_no_error(
    xt_generate_profile(
      channel,
      dem,
      extent_distance = 500,
      sample_n = 10,
      progress = TRUE
    )
  )

  expect_error(
    xt_generate_profile(
      channel,
      dem,
      extent_distance = 500,
      sample_n = 10,
      progress = "maybe"
    ),
    "progress"
  )
})

test_that("xt_generate_profile errors when transect lies in DEM nodata", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  dem <- terra::rast(
    xmin = -5,
    xmax = 5,
    ymin = -5,
    ymax = 5,
    ncols = 20,
    nrows = 20,
    crs = "EPSG:3857"
  )
  terra::values(dem) <- 100
  xy <- terra::crds(dem, na.rm = FALSE)
  thin_strip <- abs(xy[, 2]) < 0.4 & abs(xy[, 1]) < 3
  terra::values(dem)[thin_strip] <- NA_real_
  channel <- xt_as_channel(2, crs = 3857)

  expect_error(
    xt_generate_profile(
      channel,
      dem,
      extent_distance = 0,
      sample_n = 11
    ),
    "Missing DEM elevations"
  )
})

test_that("xt_generate_profile clips infinite extent to valid DEM cells", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  dem <- terra::rast(
    xmin = 0,
    xmax = 1000,
    ymin = 0,
    ymax = 1000,
    ncols = 100,
    nrows = 100,
    crs = "EPSG:3857"
  )
  terra::values(dem) <- 100
  xy <- terra::crds(dem, na.rm = FALSE)
  terra::values(dem)[xy[, 1] < 200 | xy[, 1] > 800] <- NA_real_

  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(500, 400, 500, 600), ncol = 2, byrow = TRUE)),
    crs = "EPSG:3857"
  )
  channel <- xchan:::new_channel(seg)

  expect_no_error(
    xt_generate_profile(channel, dem, extent_distance = Inf, sample_n = 50)
  )
})

test_that("xt_generate_profile works on package demo DEM", {
  skip_if_not_installed("terra")

  channel <- xt_generate_plan(demo_bankline, spacing = 200)
  dem <- terra::unwrap(demo_dem)

  expect_no_error(
    xt_generate_profile(channel, dem, sample_freq = 2)
  )
})
