test_that("xt_as_channel builds channels from widths", {
  x <- xt_as_channel(c(8, 7, 5, 6, 5, 8))
  expect_true(xt_is_channel(x))
  expect_identical(xt_n_sections(x), 6L)
  expect_s3_class(xt_axis(x), "sfc_LINESTRING")
  expect_identical(length(xt_axis(x)), 1L)
})

test_that("xt_as_channel.numeric spacing controls station placement and axis", {
  x <- xt_as_channel(c(8, 7, 5), spacing = 2)
  plan <- channel_plan(x)
  m1 <- sf::st_coordinates(plan[[1]])
  m2 <- sf::st_coordinates(plan[[2]])
  ax <- sf::st_coordinates(xt_axis(x))

  expect_equal(unname(m1[1, 2]), 0)
  expect_equal(unname(m2[1, 2]), 2)
  expect_equal(unname(ax[1, 1]), 0)
  expect_equal(unname(ax[nrow(ax), 1]), 0)
  expect_equal(unname(ax[1, 2]), 0)
  expect_equal(unname(ax[nrow(ax), 2]), 4)
})

test_that("xt_as_channel.numeric places sections evenly along supplied axis", {
  ax <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(10, 0))), crs = 3005)
  ch <- xt_as_channel(c(2, 2, 2), axis = ax)
  mids <- xchan:::plan_midpoints_sfc(channel_plan(ch))
  d <- as.numeric(sf::st_line_project(ax, mids))
  expect_equal(d, c(0, 5, 10), tolerance = 1e-8)
  expect_true(sf::st_equals(xt_axis(ch), ax, sparse = FALSE)[1, 1])
})

test_that("xt_as_channel.numeric errors when both axis and spacing are supplied", {
  ax <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(10, 0))), crs = 3005)
  expect_error(
    xt_as_channel(c(2, 2, 2), axis = ax, spacing = 2),
    "`spacing` cannot be supplied when `axis` is provided."
  )
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
