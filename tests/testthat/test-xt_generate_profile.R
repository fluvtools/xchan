test_that("xt_generate_profile errors when sampling beyond DEM extent", {
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

  expect_error(
    xt_generate_profile(
      channel,
      dem,
      extent_distance = 5,
      sample_n = 10
    ),
    "beyond the DEM extent"
  )
})

test_that("xt_generate_profile errors when DEM samples are missing", {
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
  dem[terra::cellFromXY(dem, matrix(c(0, 0), ncol = 2))] <- NA_real_
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
