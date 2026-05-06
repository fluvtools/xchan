test_that("xt_generate_plan returns a channel", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 10)
  expect_true(is_channel(ch))
  expect_identical(xt_n_sections(ch), 10L)
})
