tag_section_ids <- function(ch) {
  for (i in seq_along(ch)) {
    attr(ch[[i]], "sid") <- i
  }
  ch
}

section_ids <- function(ch) {
  vapply(
    seq_along(ch),
    function(i) attr(ch[[i]], "sid", exact = TRUE),
    integer(1)
  )
}

test_that("xt_generate_plan stores a single LINESTRING axis", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 6)
  ax <- xt_axis(ch)
  expect_s3_class(ax, "sfc_LINESTRING")
  expect_identical(length(ax), 1L)
})

test_that("xt_arrange_downstream restores canonical row order", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 8)
  set.seed(7)
  sh <- ch[sample.int(length(ch))]
  back <- xt_arrange_downstream(sh)
  expect_identical(
    sf::st_coordinates(channel_plan(ch)),
    sf::st_coordinates(channel_plan(back))
  )
})

test_that("xt_arrange_upstream reverses downstream distance order vs downstream arrange", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 8)
  set.seed(7)
  sh <- ch[sample.int(length(ch))]
  down <- xt_arrange_downstream(sh)
  up <- xt_arrange_upstream(sh)
  ds_down <- xt_distance_downstream(down)
  ds_up <- xt_distance_downstream(up)
  expect_identical(rev(ds_down), ds_up)
})

test_that("xt_arrange_downstream after xt_reverse_flow reverses section order along axis", {
  skip_if_not_installed("sf")
  ch <- tag_section_ids(xt_generate_plan(Squamish_bankline, n = 8))
  set.seed(7)
  sh <- ch[sample.int(length(ch))]
  down <- xt_arrange_downstream(sh)
  down_rev <- xt_arrange_downstream(xt_reverse_flow(sh))
  expect_identical(section_ids(down_rev), rev(section_ids(down)))
})

test_that("xt_arrange_upstream after xt_reverse_flow matches downstream arrange before reverse", {
  skip_if_not_installed("sf")
  ch <- tag_section_ids(xt_generate_plan(Squamish_bankline, n = 8))
  set.seed(7)
  sh <- ch[sample.int(length(ch))]
  down <- xt_arrange_downstream(sh)
  up_rev <- xt_arrange_upstream(xt_reverse_flow(sh))
  expect_identical(section_ids(up_rev), section_ids(down))
})

test_that("xt_elevation follows mirrored row order after reverse_flow + arrange", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 8)
  w <- as.numeric(xt_width(ch))
  prof <- lapply(seq_along(w), function(i) {
    half <- w[i] / 2
    z0 <- i * 10
    m <- matrix(c(-half, z0 - 1, 0, z0, half, z0 - 1), ncol = 2, byrow = TRUE)
    xchan:::new_profile(m, bankpoints = c(-half, half))
  })
  ch <- xchan:::set_channel_profile(ch, prof)
  set.seed(7)
  sh <- ch[sample.int(length(ch))]
  down <- xt_arrange_downstream(sh)
  down_rev <- xt_arrange_downstream(xt_reverse_flow(sh))
  z1 <- xt_elevation(down, elevation_thalweg())
  z2 <- xt_elevation(down_rev, elevation_thalweg())
  expect_equal(z2, rev(z1))
})

test_that("double xt_reverse_flow restores downstream arrange order", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 8)
  set.seed(7)
  sh <- ch[sample.int(length(ch))]
  d1 <- xt_arrange_downstream(sh)
  d2 <- xt_arrange_downstream(xt_reverse_flow(xt_reverse_flow(sh)))
  expect_identical(
    sf::st_coordinates(channel_plan(d1)),
    sf::st_coordinates(channel_plan(d2))
  )
})

test_that("xt_arrange_downstream.xchan matches arrange on full channel", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 6)
  set.seed(3)
  sh <- ch[sample.int(length(ch))]
  ax <- xt_axis(ch)
  xc2 <- xt_arrange_downstream(sh, axis = ax)
  ch2 <- xt_arrange_downstream(sh)
  expect_identical(xc2, ch2)
})
