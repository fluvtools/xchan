test_that("xt_trace_thalwegline errors without profile", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 4)
  expect_error(xt_trace_thalwegline(ch), "profile")
})

test_that("xt_trace_thalwegline returns LINESTRING for one thalweg per section", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-3, 6, 0, 3, 3, 6), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  plan <- sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(6, 0))),
    sf::st_linestring(rbind(c(0, 1), c(6, 1))),
    crs = 3857
  )
  ch <- xchan:::new_channel(plan, profile = list(p, p))
  xt_axis(ch) <- sf::st_sfc(
    sf::st_linestring(rbind(c(0, -1), c(6, -1))),
    crs = sf::st_crs(plan)
  )
  th <- xt_trace_thalwegline(ch)
  expect_identical(as.character(sf::st_geometry_type(th)), "LINESTRING")
  expect_identical(length(th), 1L)
})

test_that("xt_trace_thalwegline returns MULTILINESTRING when multiple thalwegs", {
  skip_if_not_installed("sf")
  coords <- matrix(c(-3, 5, -1, 4, 1, 4, 3, 5), ncol = 2, byrow = TRUE)
  p <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  expect_true(length(p$thalwegs) >= 2)
  plan <- sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(6, 0))),
    sf::st_linestring(rbind(c(0, 1), c(6, 1))),
    crs = 3857
  )
  ch <- xchan:::new_channel(plan, profile = list(p, p))
  xt_axis(ch) <- sf::st_sfc(
    sf::st_linestring(rbind(c(0, -1), c(6, -1))),
    crs = sf::st_crs(plan)
  )
  th <- xt_trace_thalwegline(ch)
  expect_identical(as.character(sf::st_geometry_type(th)), "MULTILINESTRING")
})
