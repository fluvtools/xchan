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
