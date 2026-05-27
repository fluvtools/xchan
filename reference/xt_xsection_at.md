# Get one cross section by downstream flow order

`xt_xsection_at()` extracts a single
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md) by
**flow order**: the `i`th section when rows are sorted by increasing
[`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md)
along the channel axis (see `axis`). Internal helper `xt_profile_at()`
returns the embedded `xs_profile` for that section (requires profile
geometry).

## Usage

``` r
xt_xsection_at(channel, i, axis = NULL)
```

## Arguments

- channel:

  A channel object
  ([`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)).

- i:

  Single positive integer: position in downstream flow order (1 =
  smallest chainage along `axis`).

- axis:

  Optional LINESTRING axis (`sfc` / `sfg`); if `NULL`, uses
  [`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md)
  on `channel`.

## Value

`xt_xsection_at()`: one
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md).

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
xt_xsection_at(channel, 1)
#> xsection
#>   Plan vertices: 2 
#>   Profile vertices: 23 
#>   Bank points: 2 
#>   Thalweg points: 1 
```
