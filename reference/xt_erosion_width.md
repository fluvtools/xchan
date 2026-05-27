# Calculate Erosion Width from Volume Change

This function calculates the erosion width for each cross-section in a
channel given a specified volume change, distributing the change
according to a given scheme.

## Usage

``` r
xt_erosion_width(channel, dv, side = "both")
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
  object.

- dv:

  Volume of erosion; single positive value or vector matching the number
  of cross sections. Plain numeric is interpreted in the channel's CRS
  length unit cubed; a
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  volume object is converted automatically (for example
  `units::set_units(c(20, 30), "L")` against a metric channel).

- side:

  A side specification controlling how widening is split between left
  and right banks. Supply either a side object from
  [`side_left()`](https://fluvtools.github.io/xchan/reference/sides.md),
  [`side_right()`](https://fluvtools.github.io/xchan/reference/sides.md),
  or
  [`side_both()`](https://fluvtools.github.io/xchan/reference/sides.md),
  or a shorthand string: `"left"`, `"right"`, or `"both"`.

## Value

A numeric vector of erosion widths for each cross-section in the
channel, carrying
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
when the channel has a CRS with a defined linear unit.

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
xt_erosion_width(channel, dv = 0.5, side = "left")
#> Units: [m]
#> [1] 0.3017721 0.3017721 0.1399481 0.1923096 0.1923096 0.1399481
xt_erosion_width(channel, dv = 0.5, side = side_left(0.75))
#> Units: [m]
#> [1] 0.3564365 0.3564365 0.1471566 0.2080922 0.2080922 0.1471566
```
