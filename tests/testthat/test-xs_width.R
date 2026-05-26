test_that("xt_width(xsection) matches plan length and parent channel slice", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(10, 12, 11), crs = 3005)
  w_ch <- as.numeric(xt_width(ch))
  for (i in seq_along(ch)) {
    expect_equal(as.numeric(xt_width(ch[[i]])), w_ch[i])
  }
})

test_that("xt_width(xs_profile) matches outer bank span", {
  coords <- matrix(c(-5, 10, 0, 5, 5, 10), ncol = 2, byrow = TRUE)
  xs <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
  expect_equal(xt_width(xs), 6)
})

test_that("xt_width(xs_profile) matches plan length after create_xs_profile", {
  original_line <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 4, 0), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  profile_data <- data.frame(
    distance = 0:10,
    elevation = c(0, 0, 5, 6, 4, 3, 4, 6, 5, 0, 0),
    x = 0:10,
    y = rep(0, 11)
  )

  profile <- xchan:::create_xs_profile(profile_data, original_line)
  expect_equal(xt_width(profile), as.numeric(sf::st_length(original_line)))
})

test_that("plan and profile widths must agree when building a channel", {
  seg <- sf::st_sfc(
    sf::st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
    crs = 3005
  )
  coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
  prof_ok <- xchan:::new_profile(coords, bankpoints = c(-1, 1))

  expect_no_error(xchan:::new_channel(seg, profile = list(prof_ok)))

  coords_wide <- matrix(c(-3, 0, 0, -1, 3, 0), ncol = 2, byrow = TRUE)
  prof_bad <- xchan:::new_profile(coords_wide, bankpoints = c(-3, 3))

  expect_error(
    xchan:::new_channel(seg, profile = list(prof_bad)),
    "Planimetric cross section length"
  )
})
