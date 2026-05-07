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

test_that("xt_widen errors by default when widening exceeds profile extent", {
  profile <- xt_profile(
    coords = matrix(
      c(
        -2, 10,
        -1, 10,
        0, 9,
        1, 10,
        2, 10
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  channel <- xt_as_channel(2, profile = list(profile))

  expect_error(
    xt_widen(channel, dw = 3, side = "left"),
    "exceeds cross section extent"
  )
})
