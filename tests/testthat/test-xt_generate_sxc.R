test_that("Cross sections can be generated.", {
  x <- xt_generate_sxc(demo_bankline, n = 10)
  expect_true(is_sxc(x))
  expect_identical(length(x), 10L)
})

