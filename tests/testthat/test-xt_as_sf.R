test_that("xt_as_sfc profile builds distance–elevation LINESTRINGs", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-1, 10, 0, 8, 1, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  ch <- xchan:::set_channel_profile(xt_as_channel(2, crs = 3005), list(prof))
  g <- xt_as_sfc(ch, what = "profile")
  expect_equal(length(g), 1L)
  expect_true(sf::st_is(g[[1]], "LINESTRING"))
})

test_that("xt_as_sfc warns for full plan extent without profiles", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(2, 2), crs = 3005)
  expect_warning(
    g <- xt_as_sfc(ch, what = "plan", extent = "full"),
    "profile"
  )
  expect_equal(length(g), length(channel_plan(ch)))
})

test_that("xt_as_sfc plan full extent spans profile horizontal range", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-5, 10, 0, 8, 5, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-2, 2))
  ch <- xchan:::set_channel_profile(xt_as_channel(4, crs = 3005), list(prof))
  g <- xt_as_sfc(ch, what = "plan", extent = "full")
  plan <- channel_plan(ch)
  len_bank <- as.numeric(sf::st_length(plan[[1]]))
  len_full <- as.numeric(sf::st_length(g[[1]]))
  expect_true(len_full >= len_bank)
})

test_that("xt_as_sfc profile extent full retains vertices outside bank span", {
  skip_if_not_installed("sf")
  coords <- matrix(
    c(-8, 10, -2, 9, 2, 9, 8, 10),
    ncol = 2,
    byrow = TRUE
  )
  prof <- xchan:::new_profile(coords, bankpoints = c(-2, 2))
  ch <- xchan:::set_channel_profile(xt_as_channel(4, crs = 3005), list(prof))
  gb <- xt_as_sfc(ch, what = "profile", extent = "banks")
  gf <- xt_as_sfc(ch, what = "profile", extent = "full")
  expect_true(
    nrow(sf::st_coordinates(gf[[1]])) > nrow(sf::st_coordinates(gb[[1]]))
  )
})
