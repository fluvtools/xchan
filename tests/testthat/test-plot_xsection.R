test_that("plot.xsection works for plan-only and profile views", {
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg)
  xs <- ch[[1]]

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(plot(xs, view = "plan"))
  expect_error(plot(xs, view = "profile"), "no profile")

  seg6 <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2)),
    crs = 3005
  )
  coords <- matrix(c(-3, 10, 0, 8, 3, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  ch2 <- xchan:::new_channel(seg6, profile = list(prof))
  xs2 <- ch2[[1]]

  expect_no_error(plot(xs2))
  expect_no_error(plot(xs2, view = "plan"))
  expect_no_error(plot(xs2, view = "profile"))
})
