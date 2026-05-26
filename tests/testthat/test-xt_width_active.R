test_that("xt_width_active on xchan is below xt_width with an island profile", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-5, 10, -2, 8, 0, 5, 2, 8, 5, 10), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, -1, 1, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))
  expect_equal(as.numeric(xt_width_active(ch)), 4)
  expect_lt(as.numeric(xt_width_active(ch)), as.numeric(xt_width(ch)))
})

test_that("xt_width_active works on xchan with multiple profile sections", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-5, 10, 0, 5, 5, 10), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 6, 1), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p, p))
  w <- xt_width_active(ch)
  expect_identical(length(w), 2L)
  expect_equal(as.numeric(w), c(6, 6))
})

test_that("xt_width_active dispatches on xs_profile and xsection", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-5, 10, -2, 8, 0, 5, 2, 8, 5, 10), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, -1, 1, 3))
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(seg, profile = list(p))
  w_ch <- as.numeric(xt_width_active(ch))
  expect_equal(as.numeric(xt_width_active(p)), w_ch)
  expect_equal(as.numeric(xt_width_active(ch[[1]])), w_ch)
})

test_that("xt_width_active equals xt_width for plan-only channels (no islands)", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(1:3, crs = 3005)
  expect_equal(as.numeric(xt_width_active(ch)), as.numeric(xt_width(ch)))
})

test_that("xt_width_active works on xsection without profile (two vertices)", {
  skip_if_not_installed("sf")
  xs <- xsection(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  expect_equal(as.numeric(xt_width_active(xs)), as.numeric(xt_width(xs)))
})

test_that("xt_width_active on plan-only xsection with four vertices (island span)", {
  skip_if_not_installed("sf")
  plan <- matrix(c(0, 0, 3, 0, 3, 1, 6, 1), ncol = 2, byrow = TRUE)
  xs <- xsection(plan)
  expect_equal(as.numeric(xt_width(xs)), 7)
  expect_equal(as.numeric(xt_width_active(xs)), 6)
})
