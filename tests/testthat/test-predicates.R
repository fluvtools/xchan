test_that("xt_is_channel and is.xchan identify xchan container", {
  skip_if_not_installed("sf")
  xc <- xt_as_channel(c(8, 7, 6))
  expect_true(xt_is_channel(xc))
  expect_true(is.xchan(xc))
  expect_false(is.xsection(xc))
})

test_that("cross-section predicates agree", {
  xs <- xsection(matrix(c(0, 0, 1, 1), ncol = 2))
  expect_true(is.xsection(xs))
  expect_true(is_xsection(xs))
  expect_true(xt_is_cross_section(xs))
  expect_false(is.xsection(1))
})

test_that("xt_has_profile works on xsection and xchan", {
  plan <- matrix(c(0, 0, 1, 0), ncol = 2)
  expect_false(xt_has_profile(xsection(plan)))

  coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  xs_prof <- xsection(plan, profile = prof)
  expect_true(xt_has_profile(xs_prof))

  xs_b <- xsection(plan + 1, profile = prof)
  expect_true(xt_has_profile(xchan(list(xs_prof, xs_b))))
  expect_false(xt_has_profile(xchan(list(xsection(plan), xsection(plan + 1)))))
})

test_that("xt_has_profile is false on xchan without profiles", {
  skip_if_not_installed("sf")
  xc <- xt_as_channel(c(4, 4))
  expect_false(xt_has_profile(xc))
})

test_that("mixed profile state across sections is rejected", {
  plan <- matrix(c(0, 0, 1, 0), ncol = 2)
  coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
  prof <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
  xs1 <- xsection(plan, profile = prof)
  xs2 <- xsection(plan + 1)
  expect_error(xchan(list(xs1, xs2)), "mixing")

  bad <- structure(
    list(xs1, xs2),
    crs = NA,
    class = c("xchan", "xchan_geom", "list")
  )
  expect_error(xt_has_profile(bad), "mixing")
})
