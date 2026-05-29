# Distance along the channel axis to cross-section stations

**`xt_distance_downstream()`** returns distance measured along the axis
**from its start** to the intersection of the axis with each cross
section’s **extended** bank-to-bank chord (the line through the first
and last plan vertices, extended if needed so it meets the axis). If
that infinite line does not intersect the axis, the chainage of the
**nearest** point on the axis to the bank midpoint is used instead.
**`xt_distance_upstream()`** returns distance along the axis **from that
station to the end** of the axis (equivalently: axis length minus
downstream distance). Together they satisfy
`xt_distance_downstream(x) + xt_distance_upstream(x) == axis_length` at
each section when lengths are numeric.

## Usage

``` r
xt_distance_downstream(channel, axis = NULL)

xt_distance_upstream(channel, axis = NULL)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  with planimetric cross sections.

- axis:

  Optional **LINESTRING** (`sfc` / `sfg`). If supplied, distances are
  measured along this line; otherwise `xt_axis(channel)` is used; if
  that is `NULL`, an error is raised (set an axis with
  `xt_axis(channel) <- ...` or use
  [`xt_generate_plan()`](https://fluvtools.github.io/xchan/reference/xt_generate_plan.md)).

## Value

A numeric vector of length `length(channel)` (same section order as
`channel`). The result carries
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
when the channel has a defined length unit (from its CRS or from manual
unit-bearing widths/profile input); plain numeric otherwise.

## Note

Use
[`xt_arrange_downstream()`](https://fluvtools.github.io/xchan/reference/xt_arrange_downstream.md)
if you need sections ordered by downstream chainage.

## Examples

``` r
# \donttest{
ch <- xt_generate_plan(squamish_bankline, n = 5)
xt_distance_downstream(ch)
#> Units: [m]
#> [1]  1124.221  3372.663  5621.105  7869.547 10117.989
xt_distance_upstream(ch)
#> Units: [m]
#> [1] 10117.989  7869.547  5621.105  3372.663  1124.221
# }
```
