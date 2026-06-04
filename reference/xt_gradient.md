# Calculate channel gradient

Compares elevations between cross sections along the channel axis (see
**Details**). A gradient is undefined for a single cross section (there
is no along-channel segment), so there is no method for
[xsection](https://fluvtools.github.io/xchan/reference/xsection.md) —
use at least two stations in an
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md).

## Usage

``` r
xt_gradient(
  channel,
  ...,
  before = 1L,
  after = 1L,
  complete = FALSE,
  elevation = elevation_bank(),
  axis = NULL
)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  with one or more cross sections (length `n`). With `n == 1`, every
  gradient is `NA` because no segment exists.

- ...:

  Must be empty.

- before:

  Number of cross sections **before** the current index to include as
  the **start** of the segment (inclusive). The gradient uses elevations
  and axis distances at indices `i - before` and `i + after` when the
  window is valid.

- after:

  Number of cross sections **after** the current index to include as the
  **end** of the segment (inclusive).

- complete:

  If `TRUE`, allow **truncated** windows at the upstream and downstream
  ends of the channel: for station `i`, the window is clipped to
  `[1, n]` so the segment runs from `max(1, i - before)` to
  `min(n, i + after)`. Every station gets a value whenever that clipped
  window spans at least two distinct stations (otherwise `NA`).

  If `FALSE`, require a **full** window: station `i` is only computed
  when \`i

  - before \>= 1`and`i + after \<=
    n`. Otherwise the value is `NA`. So with `before = 1`and`after =
    1`, only interior stations `2, ...,
    n-1`are filled; you get exactly **one**`NA\` at the front (first
    station) and **one** at the end (last station), not two at either
    boundary.

- elevation:

  Elevation specification
  ([`elevation_thalweg()`](https://fluvtools.github.io/xchan/reference/elevations.md),
  [`elevation_bank()`](https://fluvtools.github.io/xchan/reference/elevations.md),
  …).

- axis:

  Optional channel axis (same interpretation as
  [`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md);
  distances are computed without attaching units so the gradient ratio
  stays dimensionless).

## Value

Numeric vector of gradients (length matches number of cross sections),
unitless even when axis distances carry units.

## Details

At each station `i`, the gradient is
`(z_end - z_start) / (s_end - s_start)` where `z` comes from
[`xt_elevation()`](https://fluvtools.github.io/xchan/reference/xt_elevation.md)
and `s` from downstream distance along the axis (same convention as
[`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md),
stored as plain numeric for the ratio).

## Examples

``` r
channel <- xt_as_channel(rep(1, 6))
channel <- xt_add_profile(
  channel,
  distance = distance,
  elevation = elevation,
  section = id,
  banks = is_bank,
  data = profile_survey
)
gradient <- xt_gradient(channel, elevation = elevation_thalweg())

# Interior stations only (one NA first and last when before = after = 1)
gradient <- xt_gradient(channel, before = 1L, after = 1L, complete = FALSE)

# Smoothed using a wider full window
gradient <- xt_gradient(channel, before = 2L, after = 2L, elevation = elevation_bank(.f = mean))
```
