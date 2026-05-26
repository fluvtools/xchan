gully_profile <- function() {
  coords <- matrix(
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
  )
  xchan:::new_profile(coords, bankpoints = c(-1, 1))
}

test_that("Eroded volume is correct when eroding into a gully.", {
  xs <- gully_profile()
  expect_equal(erosion_volume_left(xs, 0.5), 0.75)
  expect_equal(erosion_volume_left(xs, 1), 1)
  expect_equal(erosion_volume_left(xs, 1.5), 1.25)
  expect_equal(erosion_volume_left(xs, 2), 2)
  expect_equal(erosion_volume_left(xs, 3), 4)
  expect_equal(erosion_volume_left(xs, 4), 5.75)

  dw <- 0.75
  gone_area <- erosion_volume_left(xs, dw)
  expect_lt(
    get_min_thalweg_coords(xs)[2],
    get_left_bank_coords(xs)[2]
  )
  xs <- suppressWarnings(xchan:::widen_profile_left(xs, dw))
  expect_equal(erosion_volume_left(xs, 0.25), 0.0625, tolerance = 1e-6)
  expect_equal(erosion_volume_left(xs, 0.5), 0.0625, tolerance = 1e-6)
  expect_equal(erosion_volume_left(xs, 1.5 - dw), 0.0625, tolerance = 1e-6)
  expect_equal(erosion_volume_left(xs, 2 - dw), 0.3125, tolerance = 1e-6)
  expect_equal(erosion_volume_left(xs, 3 - dw), 2.3125, tolerance = 1e-6)
  expect_equal(erosion_volume_left(xs, 4 - dw), 4.0625, tolerance = 1e-6)
})

test_that("erosion_width_left inverts erosion_volume_left on widened gully", {
  xs <- suppressWarnings(xchan:::widen_profile_left(gully_profile(), 0.75))
  target_dw <- c(0.25, 0.5, 1.25, 2.25, 3.25)
  target_v <- vapply(target_dw, erosion_volume_left, numeric(1), xs = xs)
  recovered_dw <- vapply(target_v, erosion_width_left, numeric(1), xs = xs)
  expect_equal(
    vapply(recovered_dw, erosion_volume_left, numeric(1), xs = xs),
    target_v,
    tolerance = 1e-6
  )
  expect_equal(recovered_dw[1:2], c(0.25, 0.25), tolerance = 1e-6)
  expect_equal(recovered_dw[3:5], target_dw[3:5], tolerance = 1e-6)
})

test_that("widen_profile_left increases width by dw and fixes the right bank", {
  xs <- gully_profile()
  w0 <- xt_width(xs)
  right_before <- get_right_bank_coords(xs)
  xs2 <- suppressWarnings(xchan:::widen_profile_left(xs, 2))
  expect_equal(xt_width(xs2), w0 + 2)
  expect_equal(
    unname(get_right_bank_coords(xs2)),
    unname(right_before)
  )
})
