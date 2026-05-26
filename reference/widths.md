# Width of cross sections

`xt_width()` returns geometric width. For an
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md), this is
one value per cross section from planimetric line lengths. For an
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
it is the length of that plan polyline (the same value as the
corresponding element of `xt_width()` on the parent channel). For a
single `xs_profile` object, it is the span along the profile horizontal
axis between the outermost left and right banks (the same convention as
[`xt_generate_profile()`](https://fluvtools.github.io/xchan/reference/xt_generate_profile.md)
and
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md)).

## Usage

``` r
xt_width(x, ...)

# S3 method for class 'xchan'
xt_width(x, ...)

# S3 method for class 'xsection'
xt_width(x, ...)

# S3 method for class 'xs_profile'
xt_width(x, ...)
```

## Arguments

- x:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md),
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
  or `xs_profile` object.

- ...:

  Unused (reserved for methods).

## Value

For [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md): a
numeric vector with one width per cross section, carrying
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
when the channel has a CRS with a defined linear unit (for example
metres), or when a length unit was set manually (for example via
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
widths or profile distances). When no unit is known the result is plain
numeric. For
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md): a
non-negative numeric scalar. If attribute `"crs"` is set on `x`
(unusual; the container
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) holds
CRS instead), the result may carry
[`units::units()`](https://r-quantities.github.io/units/reference/units.html)
like a channel with that CRS; otherwise plain numeric. For `xs_profile`:
a non-negative numeric scalar (no CRS context, so plain numeric).

## Examples

``` r
library(sf)
seg <- st_sfc(
  st_linestring(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)),
  crs = 3005
)
coords <- matrix(c(-1, 0, 0, -1, 1, 0), ncol = 2, byrow = TRUE)
xs <- xchan:::new_profile(coords, bankpoints = c(-1, 1))
xt_width(xs)
#> [1] 2

# xt_width(Squamish_channel)
```
