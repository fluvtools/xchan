# Widen cross sections

Widen cross sections

## Usage

``` r
xt_widen(channel, ..., dw, dv, side = "both")

# S3 method for class 'xchan'
xt_widen(channel, ..., dw, dv, side = "both")

# S3 method for class 'xsection'
xt_widen(channel, ..., dw, dv, side = "both")
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
  object.

- ...:

  Must be empty (named `dw` / `dv` arguments are required).

- dw:

  The total width to add to the channel. Positive numeric, or a
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  length object (for example `units::set_units(2, "m")`); units are
  converted to the channel's CRS length unit. Cannot be used with `dv`.

- dv:

  The total volume to remove to widen the channel. Positive numeric, or
  a
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  volume object (for example `units::set_units(50, "m^3")`); units are
  converted to the channel's CRS length unit cubed. Cannot be used with
  `dw`.

- side:

  A side specification controlling how widening is split between left
  and right banks. Supply either a side object from
  [`side_left()`](https://fluvtools.github.io/xchan/reference/sides.md),
  [`side_right()`](https://fluvtools.github.io/xchan/reference/sides.md),
  or
  [`side_both()`](https://fluvtools.github.io/xchan/reference/sides.md),
  or a shorthand string: `"left"`, `"right"`, or `"both"`.

## Value

Object of the same class as `channel`, with widened sections.

## Details

The stored channel axis
([`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md))
is **not** updated when widening: plan and profile transects move, but
the reach-scale axis polyline is left unchanged. If you set the axis to
something tied to the pre-widen plan (for example a digitized
centerline), do not expect it to refit automatically to a new
midline—that is intentional in most workflows, because the axis is used
for cross-section ordering and downstream metrics
([`xt_arrange_downstream()`](https://fluvtools.github.io/xchan/reference/xt_arrange_downstream.md),
[`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md),
etc.) rather than as a moving geometric center of each transect. To
install a different axis, use the replacement form
`xt_axis(channel) <- value` (see
[`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md)).

Profile distances are re-centered after widening so that `distance = 0`
remains the midpoint of the outer-bank pair.

## Note

The ellipsis `...` must be empty; named `dw` and `dv` keep widening
deliberate.

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
xt_widen(channel, dw = 10)
#> xchan channel with 6 cross sections.
#> <xsection 1> 20 m
#> <xsection 2> 22 m
#> <xsection 3> 18 m
#> <xsection 4> 25 m
#> <xsection 5> 21 m
#> <xsection 6> 19 m
#> With profile view
xt_widen(channel, dw = 10, side = side_left(0.75))
#> xchan channel with 6 cross sections.
#> <xsection 1> 20 m
#> <xsection 2> 22 m
#> <xsection 3> 18 m
#> <xsection 4> 25 m
#> <xsection 5> 21 m
#> <xsection 6> 19 m
#> With profile view
xt_widen(channel, dv = 5, side = "right")
#> xchan channel with 6 cross sections.
#> <xsection 1> 15 m
#> <xsection 2> 17 m
#> <xsection 3> 9.666667 m
#> <xsection 4> 17.5 m
#> <xsection 5> 13.5 m
#> <xsection 6> 10.66667 m
#> With profile view
```
