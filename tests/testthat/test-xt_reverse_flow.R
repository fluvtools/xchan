test_that("xt_reverse_flow works when channel has plan only (no profile)", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 6)
  expect_no_error(r <- xt_reverse_flow(ch))
  p0 <- ch$plan
  p1 <- r$plan
  for (i in seq_along(p0)) {
    m0 <- sf::st_coordinates(p0[i, , drop = FALSE])
    m1 <- sf::st_coordinates(p1[i, , drop = FALSE])
    n0 <- nrow(m0)
    expect_equal(m0[1, 1:2], m1[n0, 1:2])
    expect_equal(m0[n0, 1:2], m1[1, 1:2])
  }
})

test_that("xt_reverse_flow is self-inverse on plan (double reverse restores coordinates)", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 4)
  ch2 <- xt_reverse_flow(xt_reverse_flow(ch))
  expect_identical(
    sf::st_coordinates(ch$plan),
    sf::st_coordinates(ch2$plan)
  )
})

test_that("xt_reverse_flow flips profile and is self-inverse with plan+profile", {
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
  prof <- xt_profile(coords, bankpoints = c(-1, 1))
  ch <- xt_channel(.plan = seg, .profile = list(prof))
  ch2 <- xt_reverse_flow(xt_reverse_flow(ch))
  expect_identical(
    sf::st_coordinates(ch$plan),
    sf::st_coordinates(ch2$plan)
  )
  expect_identical(
    xchan:::coords_all(ch$profile[[1]]),
    xchan:::coords_all(ch2$profile[[1]])
  )
})
