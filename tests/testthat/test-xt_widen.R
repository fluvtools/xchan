test_that("Cross section widening works", {
  a <- xt_sxc(1:10)
  x <- xt_widen(a, width = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
})

test_that("Cross section widening works for sf containing sxc", {
  a <- sf::st_sf(xt_sxc(1:10))
  x <- xt_widen(a, width = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
})

test_that("Width doesn't work when sf object doesn't have sxc geom", {
  x <- sf::st_sf(demo_bankline)
  expect_error(xt_width(x))
})
