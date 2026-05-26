# Predicate tests for channel geometry and cross sections

`xt_is_channel()` is `TRUE` for the cross-section list container
(`xchan`). `xt_is_cross_section()` tests for a single cross section
(same check as
[`is.xsection()`](https://fluvtools.github.io/xchan/reference/is.xchan.md)).

## Usage

``` r
xt_is_channel(x)

xt_is_cross_section(x)
```

## Arguments

- x:

  Any object.

## Value

`TRUE` or `FALSE`.

## See also

[is.xchan](https://fluvtools.github.io/xchan/reference/is.xchan.md),
[is.xsection](https://fluvtools.github.io/xchan/reference/is.xchan.md)

## Examples

``` r
# \donttest{
xc <- xt_as_channel(c(10, 12, 11))
xt_is_channel(xc)
#> [1] TRUE
xt_is_cross_section(xc[[1]])
#> [1] TRUE
# }
```
