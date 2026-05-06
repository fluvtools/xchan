test_that("Cross section widening works", {
  a <- xt_as_channel(1:10)
  x <- xt_widen(a, dw = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
})

test_that("Cross section widening works for sf containing channel rows", {
  a <- sf::st_sf(xt_as_channel(1:10))
  x <- xt_widen(a, dw = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
})

test_that("Width doesn't work when sf object doesn't have channel geom", {
  x <- sf::st_sf(geom = fraser_bankline)
  expect_error(xt_width(x))
})
