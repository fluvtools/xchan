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

test_that("print.xchan labels subset by upstream-to-downstream when axis exists", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(rep(2, 10), crs = 3005)
  sub <- ch[c(10, 1, 5)]
  out <- testthat::capture_output(print(sub))
  lines <- grep(
    "^<xsection",
    strsplit(out, "\n", fixed = TRUE)[[1]],
    value = TRUE
  )
  expect_match(lines[1L], "<xsection 3: ID 10>", fixed = TRUE)
  expect_match(lines[2L], "<xsection 1: ID 1>", fixed = TRUE)
  expect_match(lines[3L], "<xsection 2: ID 5>", fixed = TRUE)
})

test_that("print.xchan respects n and summarizes remainder", {
  ch <- xt_as_channel(rep(1, 10))
  out <- testthat::capture_output(print(ch, n = 4))
  expect_match(out, "more cross sections", fixed = TRUE)
  out_inf <- testthat::capture_output(print(ch, n = Inf))
  expect_false(grepl("more cross sections", out_inf, fixed = TRUE))
})

test_that("xt_generate_plan clears section keys for print without : ID", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(demo_bankline, n = 12)
  expect_null(xt_section_id(ch))
  out <- testthat::capture_output(print(ch, n = 4))
  expect_false(grepl(": ID", out, fixed = TRUE))
})

test_that("print.xchan shows : ID when xt_section_id is non-default", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(demo_bankline, n = 5)
  xt_section_id(ch) <- paste0("ECCE", 22 + seq_len(length(ch)))
  out <- testthat::capture_output(print(ch, n = 3))
  expect_match(out, ": ID ECCE23", fixed = TRUE)
  expect_match(out, ": ID ECCE24", fixed = TRUE)
})
