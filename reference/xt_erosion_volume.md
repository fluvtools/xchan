# Calculate erosion volume from width change

Estimates volume removed per cross section for a given total width
increase `dw`, split between banks according to `side`.

## Usage

``` r
xt_erosion_volume(channel, dw, side = "both")

# S3 method for class 'xchan'
xt_erosion_volume(channel, dw, side = "both")

# S3 method for class 'xsection'
xt_erosion_volume(channel, dw, side = "both")

# Default S3 method
xt_erosion_volume(channel, dw, side = "both")
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
  with profile geometry.

- dw:

  Change in width; for
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
  a single positive value. For
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md), a
  single value recycled to every section or one value per cross section.
  Plain numeric uses the channel CRS length unit;
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  lengths are converted automatically.

- side:

  A side specification controlling how widening is split between left
  and right banks:
  [`side_left()`](https://fluvtools.github.io/xchan/reference/sides.md),
  [`side_right()`](https://fluvtools.github.io/xchan/reference/sides.md),
  [`side_both()`](https://fluvtools.github.io/xchan/reference/sides.md),
  or `"left"`, `"right"`, `"both"`.

## Value

For [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md), a
numeric vector of erosion volumes (one per section). For
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
length-one vector. Values carry
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
of (CRS length unit)^3 when a linear CRS unit is defined.

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
xt_erosion_volume(channel, dw = 0.5, side = "left")
#> Units: [m^3]
#> [1] 0.8799651 0.8799651 1.8799651 1.3799651 1.3799651 1.8799651
xt_erosion_volume(channel, dw = 0.5, side = side_left(0.75))
#> Units: [m^3]
#> [1] 0.7606054 0.7606054 1.7606054 1.2606054 1.2606054 1.7606054
```
