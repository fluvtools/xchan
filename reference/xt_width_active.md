# Active cross-section widths (water-filled span)

The **active** width is the portion of the cross section occupied by
water (not dry bars or islands): the sum of **water** intervals between
consecutive bank contacts. With profile geometry, bank positions come
from the profile (see
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md)
and `xs_profile`). With **planimetric geometry only**, every vertex of
the plan polyline is treated as a bank contact along the transect, in
order from left bank to right bank, alternating water / land / water
(even vertex count). For a simple two-vertex bank-to-bank segment there
are no islands and the active width equals
[`xt_width()`](https://fluvtools.github.io/xchan/reference/widths.md).

## Usage

``` r
xt_width_active(channel)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md), an
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
  or an `xs_profile`.

## Value

For [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md): a
numeric vector, one value per cross section. When the channel has a
defined length unit (CRS or manually set), values carry
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
like
[`xt_width()`](https://fluvtools.github.io/xchan/reference/widths.md);
otherwise plain numeric. For
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md) or
`xs_profile`: a single non-negative numeric (plain numeric unless the
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
carries a `"crs"` attribute with a linear unit, in which case units may
be attached).

## See also

[`xt_width()`](https://fluvtools.github.io/xchan/reference/widths.md)
