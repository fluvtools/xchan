test_that("xt_profile_at returns one xs_profile by row index", {
  coords <- matrix(c(-2, 10, 0, 8, 2, 12), ncol = 2, byrow = TRUE)
  xs1 <- xt_profile(coords, bankpoints = c(-2, 2))
  xs2 <- xt_profile(coords + matrix(c(0, 1, 0, 1, 0, 1), ncol = 2, byrow = TRUE), bankpoints = c(-2, 2))
  ch <- xt_as_channel(c(4, 4), profile = list(xs1, xs2), crs = 3005)

  out <- xt_profile_at(ch, 2)
  expect_s3_class(out, "xs_profile")
  expect_identical(out, xs2)
})

test_that("xt_profile_at errors when channel has no profiles", {
  ch <- xt_as_channel(c(4, 4), crs = 3005)
  expect_error(
    xt_profile_at(ch, 1),
    "must have profile cross sections"
  )
})

test_that("xt_profile_at validates index bounds and type", {
  coords <- matrix(c(-2, 10, 0, 8, 2, 12), ncol = 2, byrow = TRUE)
  xs <- xt_profile(coords, bankpoints = c(-2, 2))
  ch <- xt_as_channel(c(4, 4), profile = list(xs, xs), crs = 3005)

  expect_error(xt_profile_at(ch, 0), "Assertion on 'i' failed")
  expect_error(xt_profile_at(ch, 1.5), "Assertion on 'i' failed")
  expect_error(xt_profile_at(ch, 3), "out of bounds")
})
