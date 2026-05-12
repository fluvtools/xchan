test_that("xt_remove_profile.xchan strips profiles", {
  coords <- matrix(c(-1, 10, 0, 8, 1, 10), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  ch <- xchan:::set_channel_profile(xt_as_channel(c(2, 2), crs = 3005), list(prof, prof))
  expect_true(xt_has_profile(ch))
  out <- xt_remove_profile(ch)
  expect_false(xt_has_profile(out))
  expect_identical(length(out), 2L)
})

test_that("xt_remove_profile.xchan is no-op when there is no profile", {
  ch <- xt_as_channel(c(2, 2), crs = 3005)
  expect_false(xt_has_profile(ch))
  out <- xt_remove_profile(ch)
  expect_false(xt_has_profile(out))
})

test_that("xt_remove_profile.xsection clears profile slot", {
  coords <- matrix(c(-1, 0, 0, -0.2, 1, 0), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  xs <- xsection(matrix(c(-1, 0, 1, 0), ncol = 2), profile = prof)
  out <- xt_remove_profile(xs)
  expect_null(out$profile)
})

test_that("xt_remove_profile.default errors", {
  expect_error(xt_remove_profile(1L), "method for class integer")
})
