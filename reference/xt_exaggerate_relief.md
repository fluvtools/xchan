# Exaggerate relief in profile cross sections

Applies a vertical exaggeration factor to profile elevations, measured
as height above each profile's minimum elevation (thalweg baseline). Use
this to improve visual interpretation in profile and 3D views.

## Usage

``` r
xt_exaggerate_relief(x, times = 2, ...)
```

## Arguments

- x:

  An `xs_profile` or
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) object
  with profile geometry.

- times:

  Single non-negative numeric exaggeration factor. Values above 1
  increase vertical relief; values between 0 and 1 compress it.

- ...:

  Must be empty.

## Value

Object of the same class as `x`, with exaggerated profile elevations.

## Examples

``` r
# xs <- xchan:::new_profile(matrix(c(-2, 10, 0, 8, 2, 10), ncol = 2, byrow = TRUE), c(-1, 1))
# xt_exaggerate_relief(xs)
```
