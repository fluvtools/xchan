test_that("xt_as_channel builds channels from widths", {
  x <- xt_as_channel(c(8, 7, 5, 6, 5, 8))
  expect_true(xt_is_channel(x))
  expect_identical(xt_n_sections(x), 6L)
  expect_s3_class(xt_axis(x), "sfc_LINESTRING")
  expect_identical(length(xt_axis(x)), 1L)
})

test_that("xt_as_channel.numeric uses ~2 median-width spacing and vertical transects by default", {
  x <- xt_as_channel(c(8, 7, 5))
  plan <- channel_plan(x)
  m1 <- sf::st_coordinates(plan[[1]])
  m2 <- sf::st_coordinates(plan[[2]])
  ax <- sf::st_coordinates(xt_axis(x))
  spacing <- 2 * stats::median(c(8, 7, 5))

  expect_equal(unname(m1[, 1]), c(0, 0))
  expect_equal(unname(m1[, 2]), c(4, -4))
  expect_equal(unname(m2[, 1]), c(spacing, spacing))
  expect_equal(unname(ax[, 1]), c(0, spacing, 2 * spacing))
  expect_equal(unname(ax[, 2]), c(0, 0, 0))
})

test_that("xt_as_channel.numeric plan order is left bank then right (facing +x)", {
  ch <- xt_as_channel(4)
  m <- sf::st_coordinates(channel_plan(ch)[[1]])
  expect_equal(unname(m[1, 2]), 2)
  expect_equal(unname(m[2, 2]), -2)
})

test_that("xt_as_channel.numeric places sections evenly along supplied axis", {
  ax <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(10, 0))), crs = 3005)
  ch <- xt_as_channel(c(2, 2, 2), axis = ax)
  d <- xchan:::plan_chainage_on_axis(channel_plan(ch), ax)
  expect_equal(d, c(0, 5, 10), tolerance = 1e-8)
  expect_true(sf::st_equals(xt_axis(ch), ax, sparse = FALSE)[1, 1])
})

test_that("xt_as_channel.list builds a channel from xsection objects", {
  xs1 <- xsection(matrix(c(0, -1, 0, 1), ncol = 2, byrow = TRUE))
  xs2 <- xsection(matrix(c(10, -2, 10, 2), ncol = 2, byrow = TRUE))
  ch <- xt_as_channel(list(xs1, xs2))
  expect_true(xt_is_channel(ch))
  expect_identical(xt_n_sections(ch), 2L)
  expect_equal(as.numeric(xt_width(ch)), c(2, 4))
})

test_that("xt_as_channel.list errors when entries are not cross sections", {
  expect_error(xt_as_channel(list(1, 2)), "must be an `xsection`")
})

test_that("xt_as_channel.list errors on mixed profile presence", {
  coords <- matrix(c(-1, 10, 0, 8, 1, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  xs1 <- xsection(matrix(c(0, -1, 0, 1), ncol = 2, byrow = TRUE), profile = prof)
  xs2 <- xsection(matrix(c(10, -1, 10, 1), ncol = 2, byrow = TRUE))
  expect_error(xt_as_channel(list(xs1, xs2)), "either all include profile")
})

test_that("xt_as_channel normalizes sfc to LINESTRING and respects crs", {
  l <- sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2))
  x <- xt_as_channel(l, crs = 3005)
  expect_true(xt_is_channel(x))
  expect_identical(xt_n_sections(x), 1L)
  expect_identical(sf::st_crs(channel_plan(x)), sf::st_crs(3005))

  x <- xt_as_channel(sf::st_sfc(l, crs = 3005))
  expect_identical(sf::st_crs(channel_plan(x)), sf::st_crs(3005))

  x <- xt_as_channel(sf::st_sfc(l), crs = 3005)
  expect_identical(sf::st_crs(channel_plan(x)), sf::st_crs(3005))

  l2 <- sf::st_multilinestring(list(
    matrix(c(-1, 1, 0, 1), ncol = 2),
    matrix(c(-2, 1, 0, 1), ncol = 2)
  ))
  x <- xt_as_channel(l2)
  expect_true(xt_is_channel(x))
  expect_identical(xt_n_sections(x), 2L)

  sfc <- sf::st_sfc(
    l,
    sf::st_linestring(matrix(c(-1, 1, 0, 1), ncol = 2)),
    sf::st_linestring(matrix(c(-2, 1, 0, 1), ncol = 2))
  )
  x <- xt_as_channel(sfc)
  expect_identical(xt_n_sections(x), 3L)
})

test_that("xt_as_channel is stable on existing channels", {
  x <- xt_as_channel(c(8, 7, 5, 6, 5, 8))
  expect_identical(xt_as_channel(x), x)
  expect_identical(sf::st_crs(channel_plan(xt_as_channel(x, crs = 3005))), sf::st_crs(3005))
})

test_that("xt_as_channel.sfc applies crs when supplied", {
  l <- sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2))
  sfc <- sf::st_sfc(l, sf::st_linestring(matrix(c(-1, 1, 0, 1), ncol = 2)))
  x <- xt_as_channel(sfc, crs = 3005)
  expect_identical(sf::st_crs(channel_plan(x)), sf::st_crs(3005))
})
