test_that("crs_length_unit returns the expected unit symbol", {
  expect_null(xchan:::crs_length_unit(NULL))
  expect_equal(xchan:::crs_length_unit(sf::st_crs(3005)), "m")
  expect_equal(xchan:::crs_length_unit(sf::st_crs(32610)), "m")
  expect_null(xchan:::crs_length_unit(sf::st_sfc(sf::st_point(c(0, 0)))))
})

test_that("channel_length_unit falls back to length_unit attribute", {
  ch <- xt_as_channel(units::set_units(c(10, 12), "m"))
  expect_equal(xchan:::channel_length_unit(ch), "m")
  expect_null(xchan:::channel_length_unit(xt_as_channel(c(10, 12))))
})

test_that("xt_width returns units when CRS has linear units, plain numeric otherwise", {
  ch <- xt_generate_plan(demo_bankline, n = 5)
  w <- xt_width(ch)
  expect_s3_class(w, "units")
  expect_equal(units::deparse_unit(w), "m")
  expect_equal(length(w), 5L)

  ch_nocrs <- xt_as_channel(c(10, 15, 12))
  expect_type(xt_width(ch_nocrs), "double")
  expect_false(inherits(xt_width(ch_nocrs), "units"))
})

test_that("xt_distance_downstream returns units when CRS has linear units", {
  ch <- xt_generate_plan(demo_bankline, n = 5)
  ds <- xt_distance_downstream(ch)
  expect_s3_class(ds, "units")
  expect_equal(units::deparse_unit(ds), "m")
})

test_that("xt_widen accepts units inputs for dw, normalised to channel CRS unit", {
  ch <- xt_generate_plan(demo_bankline, n = 4)
  w0 <- as.numeric(xt_width(ch))

  # Plain numeric is interpreted in CRS units (metres here).
  w_num <- xt_widen(ch, dw = 2)
  expect_equal(as.numeric(xt_width(w_num)), w0 + 2)

  # units::set_units(2, "m") must give the same result.
  w_m <- xt_widen(ch, dw = units::set_units(2, "m"))
  expect_equal(as.numeric(xt_width(w_m)), w0 + 2)

  # 200 cm == 2 m.
  w_cm <- xt_widen(ch, dw = units::set_units(200, "cm"))
  expect_equal(as.numeric(xt_width(w_cm)), w0 + 2)
})

test_that("xt_widen rejects units that aren't lengths", {
  ch <- xt_generate_plan(demo_bankline, n = 3)
  expect_error(
    xt_widen(ch, dw = units::set_units(2, "kg")),
    "incompatible with the channel"
  )
})

test_that("xt_erosion_width accepts volume units, returns length units", {
  profile <- xchan:::new_profile(
    coords = matrix(
      c(
        -3,
        11,
        -2,
        10,
        -1,
        10,
        0,
        9,
        1,
        10,
        2,
        10,
        3,
        11
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  ch <- xchan:::set_channel_profile(xt_as_channel(2, crs = 3005), list(profile))

  # numeric dv (interpreted in m^3)
  out_num <- xt_erosion_width(ch, dv = 0.5)
  expect_s3_class(out_num, "units")
  expect_equal(units::deparse_unit(out_num), "m")

  # m^3 dv -> same answer
  out_m3 <- xt_erosion_width(ch, dv = units::set_units(0.5, "m^3"))
  expect_equal(as.numeric(out_m3), as.numeric(out_num))

  # 500 L == 0.5 m^3
  out_L <- xt_erosion_width(ch, dv = units::set_units(500, "L"))
  expect_equal(as.numeric(out_L), as.numeric(out_num), tolerance = 1e-9)
})

test_that("xt_erosion_volume accepts length units, returns m^3", {
  profile <- xchan:::new_profile(
    coords = matrix(
      c(
        -3,
        11,
        -2,
        10,
        -1,
        10,
        0,
        9,
        1,
        10,
        2,
        10,
        3,
        11
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  ch <- xchan:::set_channel_profile(xt_as_channel(2, crs = 3005), list(profile))

  out_num <- xt_erosion_volume(ch, dw = 0.5)
  expect_s3_class(out_num, "units")
  # `units` deparses cubic metres as "m3" (no caret); both round-trip.
  expect_equal(units::deparse_unit(out_num), "m3")

  out_m <- xt_erosion_volume(ch, dw = units::set_units(0.5, "m"))
  expect_equal(as.numeric(out_m), as.numeric(out_num))

  # 50 cm == 0.5 m
  out_cm <- xt_erosion_volume(ch, dw = units::set_units(50, "cm"))
  expect_equal(as.numeric(out_cm), as.numeric(out_num), tolerance = 1e-9)

  xc <- ch
  xs <- xc[[1]]
  expect_equal(as.numeric(xt_erosion_volume(xc, dw = 0.5)), as.numeric(out_num))
  expect_equal(as.numeric(xt_erosion_volume(xs, dw = 0.5)), as.numeric(out_num))
})

test_that("xt_widen with dv (volume) accepts units and matches plain numeric", {
  profile <- xchan:::new_profile(
    coords = matrix(
      c(
        -3,
        11,
        -2,
        10,
        -1,
        10,
        0,
        9,
        1,
        10,
        2,
        10,
        3,
        11
      ),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  ch <- xchan:::set_channel_profile(xt_as_channel(2, crs = 3005), list(profile))

  out_num <- xt_widen(ch, dv = 0.5, side = "left")
  out_m3 <- xt_widen(ch, dv = units::set_units(0.5, "m^3"), side = "left")
  expect_equal(as.numeric(xt_width(out_num)), as.numeric(xt_width(out_m3)))
})

test_that("xt_generate_plan accepts units for spacing and at", {
  bl <- demo_bankline
  ch_num <- xt_generate_plan(bl, spacing = 5000)
  ch_m <- xt_generate_plan(bl, spacing = units::set_units(5000, "m"))
  expect_equal(xt_n_sections(ch_num), xt_n_sections(ch_m))

  # 5 km == 5000 m
  ch_km <- xt_generate_plan(bl, spacing = units::set_units(5, "km"))
  expect_equal(xt_n_sections(ch_num), xt_n_sections(ch_km))

  ch_at_num <- xt_generate_plan(bl, at = c(1000, 5000, 9000))
  ch_at_m <- xt_generate_plan(
    bl,
    at = units::set_units(c(1000, 5000, 9000), "m")
  )
  expect_equal(xt_n_sections(ch_at_num), xt_n_sections(ch_at_m))
})

test_that("xt_as_channel.numeric accepts units for widths", {
  ch_num <- xt_as_channel(c(10, 12, 14), crs = 3005)
  ch_m <- xt_as_channel(units::set_units(c(10, 12, 14), "m"), crs = 3005)
  expect_equal(as.numeric(xt_width(ch_num)), as.numeric(xt_width(ch_m)))

  ch_cm <- xt_as_channel(
    units::set_units(c(1000, 1200, 1400), "cm"),
    crs = 3005
  )
  expect_equal(as.numeric(xt_width(ch_num)), as.numeric(xt_width(ch_cm)))
})

test_that("manual units on widths register without CRS", {
  ch <- xt_as_channel(units::set_units(c(10, 12, 14), "m"))
  w <- xt_width(ch)
  expect_s3_class(w, "units")
  expect_equal(units::deparse_unit(w), "m")
  out <- testthat::capture_output(print(ch))
  expect_match(out, "<xsection 1> 10 m", fixed = TRUE)
  expect_match(out, "<xsection 2> 12 m", fixed = TRUE)
})

test_that("manual units from profile_survey register without CRS", {
  ch <- xt_as_channel(rep(1, 6))
  ch <- xt_add_profile(
    ch,
    distance = distance,
    elevation = elevation,
    section = id,
    banks = is_bank,
    data = profile_survey
  )
  w <- xt_width(ch)
  expect_s3_class(w, "units")
  expect_equal(units::deparse_unit(w), "m")
  out <- testthat::capture_output(print(ch))
  expect_match(out, "<xsection 1> 10 m", fixed = TRUE)
  expect_match(out, "With profile view", fixed = TRUE)
})

test_that("xt_gradient stays unitless even when CRS carries units", {
  # Build a synthetic CRS-aware channel with a stepped thalweg so a real
  # gradient comes out. Use elevation_thalweg() (lowest profile vertex per section).
  make_profile <- function(elev_offset) {
    xchan:::new_profile(
      coords = matrix(
        c(
          -2,
          10 + elev_offset,
          -1,
          9 + elev_offset,
          0,
          8 + elev_offset,
          1,
          9 + elev_offset,
          2,
          10 + elev_offset
        ),
        ncol = 2,
        byrow = TRUE
      ),
      bankpoints = c(-1, 1)
    )
  }
  sts <- (seq_len(4L) - 1L) * 10
  plan <- sf::st_sfc(
    lapply(sts, function(x) {
      sf::st_linestring(matrix(c(x, -1, x, 1), ncol = 2, byrow = TRUE))
    }),
    crs = 3005
  )
  axis <- sf::st_sfc(
    sf::st_linestring(cbind(sts, rep(0, length(sts)))),
    crs = 3005
  )
  ch <- xchan:::new_channel(
    plan,
    axis = axis,
    profile = list(
      make_profile(3),
      make_profile(2),
      make_profile(1),
      make_profile(0)
    )
  )
  g <- xt_gradient(
    ch,
    before = 1L,
    after = 1L,
    complete = TRUE,
    elevation = elevation_thalweg()
  )
  expect_type(g, "double")
  expect_false(inherits(g, "units"))
  # Per-section elevation drops by 1 over a 10 m step → gradient ≈ -0.1.
  expect_equal(g, c(-0.1, -0.1, -0.1, -0.1))
})

test_that("xt_gradient complete=FALSE yields one NA at each end (before=after=1)", {
  make_profile <- function(elev_offset) {
    xchan:::new_profile(
      coords = matrix(
        c(
          -2,
          10 + elev_offset,
          -1,
          9 + elev_offset,
          0,
          8 + elev_offset,
          1,
          9 + elev_offset,
          2,
          10 + elev_offset
        ),
        ncol = 2,
        byrow = TRUE
      ),
      bankpoints = c(-1, 1)
    )
  }
  sts <- (seq_len(5L) - 1L) * 10
  plan <- sf::st_sfc(
    lapply(sts, function(x) {
      sf::st_linestring(matrix(c(x, -1, x, 1), ncol = 2, byrow = TRUE))
    }),
    crs = 3005
  )
  axis <- sf::st_sfc(
    sf::st_linestring(cbind(sts, rep(0, length(sts)))),
    crs = 3005
  )
  ch <- xchan:::new_channel(
    plan,
    axis = axis,
    profile = list(
      make_profile(4),
      make_profile(3),
      make_profile(2),
      make_profile(1),
      make_profile(0)
    )
  )
  g <- xt_gradient(
    ch,
    before = 1L,
    after = 1L,
    complete = FALSE,
    elevation = elevation_thalweg()
  )
  expect_equal(sum(is.na(g)), 2L)
  expect_true(is.na(g[1L]))
  expect_true(is.na(g[5L]))
  expect_false(any(is.na(g[2:4])))
})

test_that("xt_as_sfc(channel, what = '3d') preserves CRS", {
  profile <- xchan:::new_profile(
    coords = matrix(
      c(-2, 10, -1, 9, 0, 8, 1, 9, 2, 10),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-1, 1)
  )
  sts <- (seq_len(3L) - 1L) * 10
  plan <- sf::st_sfc(
    lapply(sts, function(x) {
      sf::st_linestring(matrix(c(x, -1, x, 1), ncol = 2, byrow = TRUE))
    }),
    crs = 3005
  )
  axis <- sf::st_sfc(
    sf::st_linestring(cbind(sts, rep(0, length(sts)))),
    crs = 3005
  )
  ch <- xchan:::new_channel(plan, axis = axis, profile = rep(list(profile), 3))
  g3d <- xt_as_sfc(ch, what = "3d")
  expect_equal(sf::st_crs(g3d), sf::st_crs(3005))
})
