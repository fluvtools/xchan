# Remove profile geometry from cross sections

Drops `xs_profile` data from each
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md) in
an [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md), or
from a single
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md).
Planimetric geometry and channel CRS are unchanged. Use
[`xt_as_sfc()`](https://fluvtools.github.io/xchan/reference/xt_as_sfc.md)
with `what = "profile"` when you need profile linestrings as `sfc`
before removing them.

## Usage

``` r
xt_remove_profile(x, ...)

# S3 method for class 'xchan'
xt_remove_profile(x, ...)

# S3 method for class 'xsection'
xt_remove_profile(x, ...)

# Default S3 method
xt_remove_profile(x, ...)
```

## Arguments

- x:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md).

- ...:

  Must be empty.

## Value

`x` with profile components set to `NULL`.

## Examples

``` r
ch <- xt_as_channel(c(2, 2), crs = 3005)
xt_remove_profile(ch)
#> xchan channel with 2 cross sections.
#> CRS: EPSG:3005 
#> <xsection 1> 2 m
#> <xsection 2> 2 m
```
