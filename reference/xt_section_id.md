# Cross-section identity keys on a channel

Get or set per-section **stable keys** stored on an
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md). These
keys match the `section` column when attaching tabular profiles with
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md).

## Usage

``` r
xt_section_id(channel)

# S3 method for class 'xchan'
xt_section_id(channel)

# Default S3 method
xt_section_id(channel)

xt_section_id(channel) <- value

# S3 method for class 'xchan'
xt_section_id(channel) <- value

# Default S3 method
xt_section_id(channel) <- value
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  object.

- value:

  `NULL` removes stored keys (downstream functions then treat section
  indices as `seq_len(length(channel))`). Otherwise an atomic vector of
  length `length(channel)` with **no** `NA`s and **no** duplicates.

## Value

For `xt_section_id()`, the stored vector or `NULL`. For assignment, an
updated [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md).

## Details

Keys are stored as attribute `"section_i"` on the
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) (the
name is historical).
[`xchan()`](https://fluvtools.github.io/xchan/reference/xchan.md)
initializes them to `seq_len(n)`.

## See also

[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md),
[`xchan()`](https://fluvtools.github.io/xchan/reference/xchan.md)

## Examples

``` r
ch <- xt_as_channel(c(10, 12))
xt_section_id(ch)
#> [1] 1 2
xt_section_id(ch) <- c("A12", "B04")
xt_section_id(ch)
#> [1] "A12" "B04"
xt_section_id(ch) <- NULL
xt_section_id(ch)
#> NULL
```
