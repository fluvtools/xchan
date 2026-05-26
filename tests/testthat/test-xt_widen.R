test_that("Cross section widening works", {
  a <- xt_as_channel(1:10)
  x <- xt_widen(a, dw = 2)
  expect_equal(xt_width(x), xt_width(a) + 2)
})

test_that("xt_widen side = left extends left bank on numeric xt_as_channel (+y)", {
  ch <- xt_as_channel(rep(2, 3))
  out <- xt_widen(ch, dw = 2, side = "left")
  b0 <- sf::st_coordinates(channel_plan(ch)[[2]])
  b1 <- sf::st_coordinates(channel_plan(out)[[2]])
  expect_equal(unname(b0[1, 2]), 1)
  expect_equal(unname(b1[1, 2]), 3)
})

test_that("Cross section widening dispatches for xchan", {
  geom <- xt_as_channel(1:3)
  out <- xt_widen(geom, dw = 2)
  expect_s3_class(out, "xchan")
  expect_equal(xt_width(out), xt_width(geom) + 2)
})

test_that("Cross section widening dispatches for xsection", {
  ch <- xt_as_channel(2)
  xs <- ch[[1]]

  out <- xt_widen(xs, dw = 2)
  expect_s3_class(out, "xsection")
  expect_equal(as.numeric(xt_width(out)), as.numeric(xt_width(xs)) + 2)
})

test_that("Width doesn't work when sf object doesn't have channel geom", {
  x <- sf::st_sf(geom = Squamish_bankline)
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

test_that("widen_profile_left places a vertical cliff above the left bank", {
  channel <- xt_as_channel(rep(1, 6))
  channel <- xt_add_profile(
    channel,
    distance = distance,
    elevation = elevation,
    section = id,
    banks = is_bank,
    data = profile_survey
  )
  widened <- xt_widen(channel, dw = c(5, 3, 5, 0, 0, 4))
  coords <- widened[[3]]$profile$coordinates
  lb <- get_left_bank_coords(widened[[3]]$profile)
  at_bank <- coords[abs(coords[, 1] - lb[1]) < 1e-10, , drop = FALSE]
  expect_gte(nrow(at_bank), 2L)
  expect_equal(unname(at_bank[, 1]), rep(unname(lb[1]), nrow(at_bank)))
  expect_equal(unname(at_bank[nrow(at_bank), 2]), unname(lb[2]))
  expect_false(unname(at_bank[1, 2]) == unname(lb[2]))
})

test_that("xt_widen side = left keeps right bank fixed", {
  channel <- xt_as_channel(rep(1, 6))
  channel <- xt_add_profile(
    channel,
    distance = distance,
    elevation = elevation,
    section = id,
    banks = is_bank,
    data = profile_survey
  )
  right_before <- get_right_bank_coords(channel[[1]]$profile)

  widened <- xt_widen(channel, dw = c(5, 0, 0, 0, 0, 0), side = "left")
  xs <- widened[[1]]$profile
  lb <- get_left_bank_coords(xs)
  rb <- get_right_bank_coords(xs)
  at_bank <- xs$coordinates[abs(xs$coordinates[, 1] - lb[1]) < 1e-10, , drop = FALSE]

  expect_equal(unname(rb[2]), unname(right_before[2]))
  expect_equal(unname(rb[1]), -unname(lb[1]))
  expect_gte(nrow(at_bank), 2L)
  expect_equal(unname(at_bank[, 1]), rep(unname(lb[1]), nrow(at_bank)))
})

test_that("xt_widen side = both preserves the independent left-side erosion shape", {
  channel <- xt_as_channel(rep(1, 6))
  channel <- xt_add_profile(
    channel,
    distance = distance,
    elevation = elevation,
    section = id,
    banks = is_bank,
    data = profile_survey
  )

  left_only <- xt_widen(channel[[1]], dw = 2.5, side = "left")$profile$coordinates
  both <- xt_widen(channel[[1]], dw = 5, side = "both")$profile$coordinates

  expect_equal(
    unname(left_only[1:11, 1] - left_only[1, 1]),
    unname(both[1:11, 1] - both[1, 1])
  )
  expect_equal(unname(left_only[1:11, 2]), unname(both[1:11, 2]))
})

test_that("xt_widen preserves inner banks and wetted thalwegs for island profiles", {
  prof <- xchan:::new_profile(
    matrix(
      c(
        1, -1,
        2, 0,
        3, 1,
        4, 2,
        5, 1,
        6, 1,
        7, 0,
        8, 1,
        9, 2,
        10, 1,
        11, 0.5,
        12, 1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(6, 8, 10, 12)
  )
  xs <- xsection(
    matrix(
      c(
        6, 1,
        8, 1,
        10, 1,
        12, 1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    profile = prof
  )

  out <- xt_widen(xs, dw = 1, side = "left")$profile

  expect_equal(unname(get_bank_distances(out)), c(-3.5, -0.5, 1.5, 3.5))
  expect_equal(unname(get_thalweg_distances(out)), c(-2.5, -1.5))

  out2 <- xt_widen(xs, dw = 2, side = "left")$profile
  expect_equal(unname(get_bank_distances(out2)), c(-4, 0, 2, 4))
  expect_equal(unname(get_thalweg_distances(out2)), c(-3, -1))
  lb <- get_left_bank_coords(out2)
  expect_equal(unname(lb[2]), 1)
  at_bank <- out2$coordinates[abs(out2$coordinates[, 1] - lb[1]) < 1e-10, , drop = FALSE]
  expect_equal(nrow(at_bank), 2L)
  expect_equal(unname(at_bank[nrow(at_bank), 2]), 1)
  expect_equal(unname(at_bank[1, 2]), 2)
})

test_that("xt_widen into floodplain depression preserves left-side channel shape", {
  prof <- xchan:::new_profile(
    matrix(
      c(
        1, -1,
        2, 0,
        3, 1,
        4, 2,
        5, 1,
        6, 1,
        7, 0,
        8, 1,
        9, 2,
        10, 1,
        11, 0.5,
        12, 1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(6, 8, 10, 12)
  )
  xs <- xsection(
    matrix(c(6, 1, 8, 1, 10, 1, 12, 1), ncol = 2, byrow = TRUE),
    profile = prof
  )

  out <- suppressWarnings(xt_widen(xs, dw = 3.5, side = "left")$profile$coordinates)

  expect_true(any(abs(out[, 1] - (-0.25)) < 1e-10 & abs(out[, 2]) < 1e-10))
  expect_equal(
    unname(out[abs(out[, 1] - (-4.75)) < 1e-10, ]),
    matrix(c(-4.75, 0.5, -4.75, 1), ncol = 2, byrow = TRUE)
  )
  expect_equal(unname(out[abs(out[, 1] - (-3.75)) < 1e-10, ]), c(-3.75, 0))

  out4 <- suppressWarnings(xt_widen(xs, dw = 4, side = "left")$profile$coordinates)
  expect_equal(
    unname(out4[abs(out4[, 1] - (-5)) < 1e-10, ]),
    matrix(c(-5, 0, -5, 1), ncol = 2, byrow = TRUE)
  )
  expect_equal(unname(out4[abs(out4[, 1] - (-4)) < 1e-10, ]), c(-4, 0))
  expect_true(any(abs(out4[, 1]) < 1e-10 & abs(out4[, 2]) < 1e-10))

  thal4 <- suppressWarnings(xt_widen(xs, dw = 4, side = "left")$profile)
  expect_equal(unname(get_thalweg_distances(thal4)), c(-4, 0))
})

test_that("widen_profile_left shifts the left-side channel with the bank", {
  xs <- xchan:::new_profile(
    coords = matrix(
      c(
        -5, 10.5,
        -4, 11,
        -3, 11,
        -2, 9,
        -1, 11,
        0, 10,
        1, 11
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  dw <- 0.75
  left_before <- get_left_bank_coords(xs)
  thal_before <- min(get_thalweg_distances(xs))
  x_new <- left_before[1] - dw

  widened <- suppressWarnings(xchan:::widen_profile_left(xs, dw))
  at_new_bank <- widened$coordinates[
    abs(widened$coordinates[, 1] - x_new) < 1e-10,
    ,
    drop = FALSE
  ]

  expect_equal(min(get_thalweg_distances(widened)), thal_before - dw)
  expect_equal(sort(get_thalweg_distances(widened)), c(thal_before - dw, thal_before))
  expect_equal(nrow(at_new_bank), 2L)
  expect_equal(
    sort(unname(at_new_bank[, 2])),
    sort(c(coords_interpolate(xs$coordinates, x_new)[2], left_before[2]))
  )
})

test_that("widen_profile lengthens the flat thalweg strip without stretching flanks", {
  xs <- xchan:::new_profile(
    coords = matrix(
      c(
        -4, 12,
        -2, 11,
        -1.5, 10,
        -1, 9,
        1, 9,
        1.5, 10,
        2, 11,
        4, 12
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-2, 2)
  )

  widened <- xchan:::widen_profile(xs, dw = 2, prop_left = 0.5)
  coords <- widened$coordinates

  expect_true(any(coords[, 1] == -2 & coords[, 2] == 9))
  expect_true(any(coords[, 1] == 2 & coords[, 2] == 9))
  expect_equal(sort(get_thalweg_distances(widened)), c(-2, 2))
})

test_that("xt_widen errors by default when widening exceeds profile extent", {
  profile <- xchan:::new_profile(
    coords = matrix(
      c(
        -2,
        10,
        -1,
        10,
        0,
        9,
        1,
        10,
        2,
        10
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  channel <- xchan:::set_channel_profile(
    xt_as_channel(2, crs = 3005),
    list(profile)
  )

  expect_error(
    xt_widen(channel, dw = 3, side = "left"),
    "exceeds cross section extent"
  )
})
