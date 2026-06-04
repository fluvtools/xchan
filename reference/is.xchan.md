# Predicate tests for `xchan` and `xsection`

These functions test inheritance from a single class each. For list
containers holding cross sections, see
[`xt_is_channel()`](https://fluvtools.github.io/xchan/reference/xt_is_channel.md)
and
[`xt_is_cross_section()`](https://fluvtools.github.io/xchan/reference/xt_is_channel.md).

## Usage

``` r
is.xchan(x)

is.xsection(x)

is_xsection(x)
```

## Arguments

- x:

  Any object.

## Value

`TRUE` or `FALSE`.

## See also

[xt_is_channel](https://fluvtools.github.io/xchan/reference/xt_is_channel.md),
[xt_is_cross_section](https://fluvtools.github.io/xchan/reference/xt_is_channel.md)

## Examples

``` r
# \donttest{
xc <- xt_as_channel(c(10, 12, 11))
is.xchan(xc)
#> [1] TRUE
xs <- xc[[1]]
is.xsection(xs)
#> [1] TRUE
is_xsection(xs)
#> [1] TRUE
# }
```
