# Print a channel object

Print a channel object

## Usage

``` r
# S3 method for class 'xchan'
print(x, ..., n = 6)
```

## Arguments

- x:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md).

- ...:

  Ignored.

- n:

  Maximum number of cross sections to print, in **list order** (same as
  `[[1]]`, `[[2]]`, …). When a channel axis is available
  ([`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md)),
  each line is prefixed with that section’s **upstream-to-downstream**
  index among the sections being printed (`1` = most upstream, `n` =
  most downstream); otherwise indices are `1` … `n` in list order. If
  [`xt_section_id()`](https://fluvtools.github.io/xchan/reference/xt_section_id.md)
  is set to a vector of length `length(x)` and it is **not** exactly the
  consecutive integers `1`, `2`, …, `n` in list order, each printed line
  also includes `: ID <key>` for that list position’s key. The default
  is `6`. Use `Inf` to print every section.
