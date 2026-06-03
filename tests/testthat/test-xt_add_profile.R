test_that("xt_add_profile snap plan attaches matching profile widths", {
  skip_if_not_installed("sf")
  plan <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 2, 1), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(plan)
  df <- data.frame(
    cs = c(1L, 1L, 1L, 2L, 2L, 2L),
    d = c(-1, 0, 1, -1, 0, 1),
    z = c(5, 4, 5, 6, 3, 6),
    bk = rep(c(TRUE, FALSE, TRUE), 2)
  )
  out <- xt_add_profile(
    ch,
    distance = d,
    elevation = z,
    section = cs,
    banks = bk,
    data = df,
    snap_banks_to = "plan"
  )
  expect_equal(as.numeric(xt_width(out)), c(2, 2))
  expect_identical(out[[1]]$profile$coordinates[, 2], c(5, 4, 5))
})

test_that("xt_add_profile snap profile widens plan to survey span", {
  skip_if_not_installed("sf")
  plan <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 2, 1), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(plan)
  df <- data.frame(
    cs = c(1L, 1L, 1L, 2L, 2L, 2L),
    d = c(-1.5, 0, 1.5, -1.5, 0, 1.5),
    z = c(5, 4, 5, 6, 3, 6),
    bk = rep(c(TRUE, FALSE, TRUE), 2)
  )
  out <- xt_add_profile(
    ch,
    distance = d,
    elevation = z,
    section = cs,
    banks = bk,
    data = df,
    snap_banks_to = "profile"
  )
  expect_equal(as.numeric(xt_width(out)), c(3, 3))
  expect_equal(as.numeric(sf::st_length(channel_plan(out))), c(3, 3))
})

test_that("xt_add_profile matches section_i keys when present", {
  skip_if_not_installed("sf")
  plan <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 2, 1), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch0 <- xchan:::new_channel(plan)
  ch <- ch0[c(2, 1)]
  df <- data.frame(
    cs = c(2L, 2L, 2L, 1L, 1L, 1L),
    d = c(-1, 0, 1, -1, 0, 1),
    z = c(9, 8, 9, 5, 4, 5),
    bk = rep(c(TRUE, FALSE, TRUE), 2)
  )
  out <- xt_add_profile(
    ch,
    distance = d,
    elevation = z,
    section = cs,
    banks = bk,
    data = df,
    snap_banks_to = "plan"
  )
  expect_equal(out[[1L]]$profile$coordinates[, 2], c(9, 8, 9))
  expect_equal(out[[2L]]$profile$coordinates[, 2], c(5, 4, 5))
})

test_that("xt_add_profile.xsection works without data", {
  skip_if_not_installed("sf")
  xs <- xsection(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE))
  d <- c(-1, 0, 1)
  z <- c(5, 4, 5)
  b <- c(TRUE, FALSE, TRUE)
  out <- xt_add_profile(
    xs,
    distance = d,
    elevation = z,
    banks = b,
    snap_banks_to = "plan"
  )
  expect_equal(as.numeric(xt_width(out)), 2)
})

test_that("xt_add_profile.xchan works with default data = NULL in caller env", {
  skip_if_not_installed("sf")
  plan <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 2, 1), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  ch <- xchan:::new_channel(plan)
  cs <- c(1L, 1L, 1L, 2L, 2L, 2L)
  d <- c(-1, 0, 1, -1, 0, 1)
  z <- c(5, 4, 5, 6, 3, 6)
  bk <- rep(c(TRUE, FALSE, TRUE), 2)
  out <- xt_add_profile(
    ch,
    distance = d,
    elevation = z,
    section = cs,
    banks = bk,
    snap_banks_to = "plan"
  )
  expect_equal(as.numeric(xt_width(out)), c(2, 2))
  expect_identical(out[[1]]$profile$coordinates[, 2], c(5, 4, 5))
})
