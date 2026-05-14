test_that("print.xchan shows channel summary and cross section widths", {
  ch <- xt_as_channel(c(10, 12, 8))
  out <- testthat::capture_output(print(ch))
  expect_match(out, "xchan channel with 3 cross sections", fixed = TRUE)
  expect_match(out, "<xsection 1> 10 (-)", fixed = TRUE)
  expect_match(out, "<xsection 2> 12 (-)", fixed = TRUE)
  expect_match(out, "<xsection 3> 8 (-)", fixed = TRUE)
})

test_that("print.xchan shows CRS length unit when widths are units", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(10, 12), crs = 3005)
  out <- testthat::capture_output(print(ch))
  expect_match(out, "<xsection 1> 10 m", fixed = TRUE)
  expect_match(out, "<xsection 2> 12 m", fixed = TRUE)
})

test_that("print.xchan shows section_i labels after subset", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(rep(5, 40), crs = 3005)
  sub <- ch[c(3, 2, 1, 5, 7)]
  out <- testthat::capture_output(print(sub, n = 4))
  expect_match(out, "<xsection 3>", fixed = TRUE)
  expect_match(out, "<xsection 2>", fixed = TRUE)
  expect_match(out, "<xsection 1>", fixed = TRUE)
  expect_match(out, "<xsection 5>", fixed = TRUE)
})

test_that("print.xchan respects n and summarizes remainder", {
  ch <- xt_as_channel(rep(1, 10))
  out <- testthat::capture_output(print(ch, n = 4))
  expect_match(out, "more cross sections", fixed = TRUE)
  out_inf <- testthat::capture_output(print(ch, n = Inf))
  expect_false(grepl("more cross sections", out_inf, fixed = TRUE))
})
