# Sort channel rows by distance along the axis

`xt_arrange_downstream()` permutes cross sections so they follow
**increasing** projected distance along `axis` from the axis **start**
([`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md)).
After
[`xt_reverse_flow()`](https://fluvtools.github.io/xchan/reference/xt_reverse_flow.md),
the stored axis (if any) is reversed, so that zero chainage is at the
opposite end of the same geographic line — increasing distance is then
hydrologic downstream with reversed flow. `xt_arrange_upstream()` uses
**decreasing** distance from the axis start.

## Usage

``` r
xt_arrange_downstream(channel, axis = NULL)

xt_arrange_upstream(channel, axis = NULL)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  object.

- axis:

  Optional LINESTRING (`sfc` / `sfg`) for projection ordering (same
  rules as
  [`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md)).

## Value

An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) with
sections reordered along the axis (attributes `crs` and `axis`
preserved).

## Details

Ordering uses the same chainage as
[`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md):
intersection of the extended bank-to-bank chord with the axis (nearest
intersection to the bank midpoint when there are several), otherwise the
nearest point on the axis to the bank midpoint.

**Reverse flow:**
[`xt_reverse_flow()`](https://fluvtools.github.io/xchan/reference/xt_reverse_flow.md)
does not permute sections but reverses the stored axis, so
`xt_arrange_downstream()` orders sections for the new downstream
direction.
[`elevation_bank()`](https://fluvtools.github.io/xchan/reference/elevations.md)
with default `min` is unchanged until you reorder.

## See also

[`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md),
[`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md),
[`xt_reverse_flow()`](https://fluvtools.github.io/xchan/reference/xt_reverse_flow.md)

## Examples

``` r
# \donttest{
ch <- xt_generate_plan(squamish_bankline, n = 15)
#> Warning: Inf replaced by maximum positive value
#> Warning: Inf replaced by maximum positive value
#> Warning: Inf replaced by maximum positive value
#> Warning: Inf replaced by maximum positive value
#> Warning: Inf replaced by maximum positive value
#> Warning: Inf replaced by maximum positive value
#> Warning: Inf replaced by maximum positive value
ch_down <- xt_arrange_downstream(ch)
ch_up <- xt_arrange_upstream(ch)
# }
```
