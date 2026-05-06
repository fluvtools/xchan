test_that("xt_as_channel builds channels from widths", {
  x <- xt_as_channel(c(8, 7, 5, 6, 5, 8))
  expect_true(is_channel(x))
  expect_identical(xt_n_sections(x), 6L)
})

test_that("xt_as_channel normalizes sfc to LINESTRING and respects crs", {
  l <- sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2))
  x <- xt_as_channel(l, crs = 3005)
  expect_true(is_channel(x))
  expect_identical(xt_n_sections(x), 1L)
  expect_identical(sf::st_crs(xt_column_plan(x)), sf::st_crs(3005))

  x <- xt_as_channel(sf::st_sfc(l, crs = 3005))
  expect_identical(sf::st_crs(xt_column_plan(x)), sf::st_crs(3005))

  x <- xt_as_channel(sf::st_sfc(l), crs = 3005)
  expect_identical(sf::st_crs(xt_column_plan(x)), sf::st_crs(3005))

  l2 <- sf::st_multilinestring(list(
    matrix(c(-1, 1, 0, 1), ncol = 2),
    matrix(c(-2, 1, 0, 1), ncol = 2)
  ))
  x <- xt_as_channel(l2)
  expect_true(is_channel(x))
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
  expect_identical(sf::st_crs(xt_column_plan(xt_as_channel(x, crs = 3005))), sf::st_crs(3005))
})
