# Channel bank footprint (polygon)

Get or set the plan-view channel footprint stored on an
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md). When
present (for example after
[`xt_generate_plan()`](https://fluvtools.github.io/xchan/reference/xt_generate_plan.md)),
it can be drawn under transects in
[`plot.xchan()`](https://fluvtools.github.io/xchan/reference/plot.xchan.md).

## Usage

``` r
xt_bankline(channel)

# S3 method for class 'xchan'
xt_bankline(channel)

# Default S3 method
xt_bankline(channel)

xt_bankline(channel) <- value

# S3 method for class 'xchan'
xt_bankline(channel) <- value

# Default S3 method
xt_bankline(channel) <- value
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  object.

- value:

  `NULL` clears the footprint. Otherwise an `sfc` or `sfg` with only
  **POLYGON** / **MULTIPOLYGON** geometries, in the same CRS as the plan
  (or transformed with a warning).

## Value

For `xt_bankline()`, the stored `sfc` or `NULL`. For assignment, an
updated [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
with attribute `bankline`.

## Details

The footprint is stored as attribute `"bankline"` on the
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md).

## See also

[`xt_generate_plan()`](https://fluvtools.github.io/xchan/reference/xt_generate_plan.md),
[`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md)

## Examples

``` r
# \donttest{
library(sf)
ch <- xt_generate_plan(squamish_bankline, n = 12)
bl <- xt_bankline(ch)
plot(bl)

# }
```
