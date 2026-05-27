# Get elevation values using an elevation specification

Get elevation values using an elevation specification

## Usage

``` r
xt_elevation(channel, reference, ...)

# S3 method for class 'xchan'
xt_elevation(channel, reference, ...)

# S3 method for class 'xsection'
xt_elevation(channel, reference, ...)

# Default S3 method
xt_elevation(channel, reference, ...)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
  object. Single-section inputs are wrapped for evaluation.

- reference:

  An elevation specification from
  [`elevation_thalweg()`](https://fluvtools.github.io/xchan/reference/elevations.md)
  and related helpers (class `"xchan_elevation"`).

- ...:

  Reserved for methods (must be empty).

## Value

A numeric vector of elevations, one per cross section
([`length()`](https://rdrr.io/r/base/length.html) for
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md), or `1`
for a single
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)).

**Order:** Values follow **storage order only** — position `i` is always
`[[i]]` of the
[`xchan()`](https://fluvtools.github.io/xchan/reference/xchan.md) list.
There is **no** sorting inside `xt_elevation()`. Reorder sections first
(e.g.
[`xt_arrange_downstream()`](https://fluvtools.github.io/xchan/reference/xt_arrange_downstream.md)
for increasing chainage along the axis from its current start; after
[`xt_reverse_flow()`](https://fluvtools.github.io/xchan/reference/xt_reverse_flow.md),
the stored axis is reversed so that sort follows hydrologic downstream).

## Details

Elevation specifications read each cross section's profile from the
geometry container; profile geometry must exist when the chosen
reference requires it.

## Examples

``` r
channel <- xt_as_channel(rep(1, 6))
channel <- xt_add_profile(
  channel,
  distance = distance,
  elevation = elevation,
  section = id,
  banks = is_bank,
  data = profile_survey
)
elevations <- xt_elevation(channel, reference = elevation_thalweg())

# One section
xs <- channel[[1]]
z <- xt_elevation(xs, reference = elevation_thalweg())
```
