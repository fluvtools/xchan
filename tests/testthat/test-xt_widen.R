test_that("Cross section widening works", {
  a <- xt_as_channel(1:10)
  x <- xt_widen(a, dw = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
})

test_that("Cross section widening works for channel with extra columns", {
  a <- xt_as_channel(1:10, id = seq_len(10))
  x <- xt_widen(a, dw = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
  expect_identical(x$id, a$id)
})

test_that("Cross section widening dispatches for xchan", {
  ch <- xt_as_channel(1:3)
  xcol <- attr(ch, "xsection_col", exact = TRUE)
  geom <- ch[[xcol]]

  out <- xt_widen(geom, dw = 2)
  expect_s3_class(out, "xchan")
  expect_equal(xt_width(out), xt_width(geom) + 2)
})

test_that("Cross section widening dispatches for xsection", {
  ch <- xt_as_channel(2)
  xcol <- attr(ch, "xsection_col", exact = TRUE)
  xs <- ch[[xcol]][[1]]

  out <- xt_widen(xs, dw = 2)
  expect_s3_class(out, "xsection")
  out_width <- as.numeric(sf::st_length(xsection_to_linestring(out)))
  in_width <- as.numeric(sf::st_length(xsection_to_linestring(xs)))
  expect_equal(out_width, in_width + 2)
})

test_that("Width doesn't work when sf object doesn't have channel geom", {
  x <- sf::st_sf(geom = fraser_bankline)
  expect_error(xt_width(x))
})

test_that("widen_plan increases path length by dw for bent plan lines", {
  # Non-collinear vertices: length must grow along the bank tangent (final
  # segment), not the first-to-last chord, or st_length(plan) changes by the wrong amount.
  skip_if_not_installed("sf")
  plan <- sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(100, 0), c(100, 50))),
    crs = 3005
  )
  w0 <- as.numeric(sf::st_length(plan[[1]]))
  out <- xchan:::widen_plan(plan, dw = 10, prop_left = 0.5)
  w1 <- as.numeric(sf::st_length(out[[1]]))
  expect_equal(w1, w0 + 10)
})

test_that("xt_widen errors by default when widening exceeds profile extent", {
  profile <- xchan:::new_profile(
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
