test_that("Channel width works", {
  w <- c(8, 7, 5, 6, 5, 8)
  x <- xt_as_channel(w)
  expect_equal(xt_width(x), w)

  l <- sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  x <- xt_as_channel(l, crs = 3005)
  # CRS-aware widths now carry units (m for EPSG:3005); compare bare values.
  expect_equal(
    as.numeric(xt_width(x)),
    rep(sqrt(2), length(xt_width(x)))
  )
})
