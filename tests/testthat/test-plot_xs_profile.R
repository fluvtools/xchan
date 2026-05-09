test_that("plot.xs_profile allows from or to independently", {
  coords <- matrix(c(-5, 10, 0, 8, 5, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(prof, from = -10))
  expect_no_error(plot(prof, to = 10))
  expect_no_error(plot(prof, from = -100, to = 100))
})

test_that("plot.xs_profile does not mutate the original profile (exaggerate)", {
  coords <- matrix(c(-5, 10, 0, 8, 5, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  elev <- prof$coordinates[, 2]
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(prof, exaggerate = 5)
  expect_equal(prof$coordinates[, 2], elev)
  plot(prof, exaggerate = 1)
  expect_equal(prof$coordinates[, 2], elev)
})

test_that("plot.xs_profile errors when implied horizontal range is invalid", {
  coords <- matrix(c(-5, 10, 0, 8, 5, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(prof, from = 100, to = 0), "empty")
})
