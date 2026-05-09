test_that("xt_reverse_flow preserves thalweg elevations from elevation_thalweg()", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 6)
  w <- as.numeric(xt_width(ch))
  prof <- lapply(seq_along(w), function(i) {
    half <- w[i] / 2
    z0 <- 100 + i
    m <- matrix(c(-half, z0 + 2, 0, z0, half, z0 + 2), ncol = 2, byrow = TRUE)
    xchan:::new_profile(m, bankpoints = c(-half, half))
  })
  ch <- xchan:::set_channel_profile(ch, prof)
  z1 <- xt_elevation(ch, elevation_thalweg())
  z2 <- xt_elevation(xt_reverse_flow(ch), elevation_thalweg())
  expect_equal(z2, z1)
})

test_that("xt_reverse_flow works when channel has plan only (no profile)", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 6)
  expect_no_error(r <- xt_reverse_flow(ch))
  p0 <- channel_plan(ch)
  p1 <- channel_plan(r)
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
    sf::st_coordinates(channel_plan(ch)),
    sf::st_coordinates(channel_plan(ch2))
  )
})

test_that("xt_reverse_flow flips profile and is self-inverse with plan+profile", {
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  ch <- xchan:::new_channel(seg, profile = list(prof))
  ch2 <- xt_reverse_flow(xt_reverse_flow(ch))
  expect_identical(
    sf::st_coordinates(channel_plan(ch)),
    sf::st_coordinates(channel_plan(ch2))
  )
  expect_identical(
    xchan:::coords_all(channel_profile(ch)[[1]]),
    xchan:::coords_all(channel_profile(ch2)[[1]])
  )
})
