test_that("xt_generate_plan returns a channel", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 10)
  expect_true(is_channel(ch))
  expect_identical(xt_n_sections(ch), 10L)
})

test_that("planimetric segments orient first vertex to left bank (downstream)", {
  skip_if_not_installed("sf")
  library(sf)
  bl <- sf::st_sfc(fraser_bankline, crs = 3005)
  cl <- banks_to_centerline(bl)
  len <- as.numeric(sum(sf::st_length(cl)))
  n <- 12L
  ch <- xt_generate_plan(fraser_bankline, n = n)
  plan <- ch$plan
  pts <- sf::st_line_sample(cl, density = n / len)
  pts <- pts[!vapply(pts, sf::st_is_empty, logical(1))]
  pts <- sf::st_cast(pts, "POINT")
  dists <- sf::st_line_project(cl, pts)
  pts <- pts[order(dists)]
  for (i in seq_len(n)) {
    s <- as.numeric(sf::st_line_project(cl, pts[i]))
    eps <- max(len * 1e-10, 1e-4)
    s0 <- max(0, s - eps)
    s1 <- min(len, s + eps)
    p0 <- sf::st_line_interpolate(cl, s0)
    p1 <- sf::st_line_interpolate(cl, s1)
    m0 <- sf::st_coordinates(p0)[1L, 1:2]
    m1 <- sf::st_coordinates(p1)[1L, 1:2]
    t <- m1 - m0
    t <- t / sqrt(sum(t^2))
    m <- sf::st_coordinates(plan[i])
    e1 <- m[1L, 1:2]
    ctr <- sf::st_coordinates(pts[i])[1L, 1:2]
    cross1 <- t[1L] * (e1[2L] - ctr[2L]) - t[2L] * (e1[1L] - ctr[1L])
    expect_gt(cross1, 0)
  }
})
