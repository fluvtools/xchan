test_that("elevation_bank_left and elevation_bank_right return outer bank z", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-4, 12, 0, 5, 4, 9), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-4, 4))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 8, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))

  z_left <- xt_elevation(ch, elevation_bank_left())
  z_right <- xt_elevation(ch, elevation_bank_right())

  expect_equal(unname(z_left), xchan:::get_left_bank_coords(p)[2])
  expect_equal(unname(z_right), xchan:::get_right_bank_coords(p)[2])
})

test_that("elevation_bank() applies .f to outer bank elevations", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-4, 12, 0, 5, 4, 9), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-4, 4))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 8, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))
  z_banks <- c(
    xchan:::get_left_bank_coords(p)[2],
    xchan:::get_right_bank_coords(p)[2]
  )

  expect_equal(
    unname(xt_elevation(ch, elevation_bank())),
    min(z_banks)
  )
  expect_equal(
    unname(xt_elevation(ch, elevation_bank(.f = mean))),
    mean(z_banks)
  )
  expect_s3_class(elevation_bank(), "xchan_elevation")
  expect_s3_class(elevation_bank_left(), "xchan_elevation")
  expect_s3_class(elevation_bank_right(), "xchan_elevation")
})

test_that("elevation_bed(.f = mean) averages elevations between outer banks", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-3, 10, 0, 5, 3, 10), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))
  z <- xt_elevation(ch, elevation_bed(.f = mean))
  expect_equal(unname(z), mean(p$coordinates[, 2]))
})

test_that("elevation_bed(.f = min) matches minimum bed elevation in span", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-5, 10, -2, 8, 0, 5, 2, 8, 5, 10), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))
  z <- xt_elevation(ch, elevation_bed(.f = min))
  expect_equal(unname(z), 5)
})

test_that("elevation_bed excludes dry island interior (water intervals only)", {
  skip_if_not_installed("sf")
  coords <- matrix(
    c(
      -3, 5, -2, 4, -1, 5,
      0, 100,
      1, 5, 2, 4, 3, 5
    ),
    ncol = 2,
    byrow = TRUE
  )
  p <- xchan:::new_profile(coords, bankpoints = c(-3, -1, 1, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))
  d <- p$coordinates[, 1]
  bd <- xchan:::get_bank_distances(p)
  full_span <- d >= min(bd) & d <= max(bd)
  mean_full <- mean(p$coordinates[full_span, 2])
  z_bed <- as.numeric(xt_elevation(ch, elevation_bed(.f = mean)))
  expect_lt(z_bed, mean_full)
})
