test_that("xt_generate_plan stores a single LINESTRING axis", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 6)
  ax <- xt_axis(ch)
  expect_s3_class(ax, "sfc_LINESTRING")
  expect_identical(length(ax), 1L)
})

test_that("xt_trace_centerline is invariant to row shuffle when axis is stored", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 9)
  tr <- xt_trace_centerline(ch)
  set.seed(11)
  sh <- ch[sample.int(nrow(ch)), ]
  expect_true(sf::st_equals(tr, xt_trace_centerline(sh), sparse = FALSE)[1L, 1L])
})

test_that("xt_arrange_downstream restores canonical row order", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 8)
  set.seed(7)
  sh <- ch[sample.int(nrow(ch)), ]
  back <- xt_arrange_downstream(sh)
  expect_identical(sf::st_coordinates(ch$plan), sf::st_coordinates(back$plan))
})

test_that("xt_trace_centerline requires an axis when none is supplied/stored", {
  skip_if_not_installed("sf")
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(-1, 1, 1, 1), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xt_channel(.plan = seg)
  expect_error(xt_trace_centerline(ch), "No axis stored")
})
