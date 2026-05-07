test_that("create_xs_profile aligns banks and thalweg to channel window", {
  original_line <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 4, 0), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  profile_data <- data.frame(
    distance = 0:10,
    elevation = c(0, 0, 5, 6, 4, 3, 4, 6, 5, 0, 0),
    x = 0:10,
    y = rep(0, 11)
  )

  profile <- xchan:::create_xs_profile(profile_data, original_line)

  expect_equal(unname(profile$coordinates[profile$banks, 1]), c(-2, 2))
  expect_equal(unname(profile$coordinates[profile$thalwegs, 1]), 0)
})
