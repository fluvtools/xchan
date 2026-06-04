# Reverse a Channel's Flow Direction

Swaps what is called the "left" and "right" banks (from the perspective
of someone looking downstream).

## Usage

``` r
xt_reverse_flow(channel)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  object.

## Value

An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) with
flow direction reversed (see **Details**).

## Details

Planimetric segments are reversed end-for-end (`flip_plan()`), so the
first vertex now corresponds to what was the right bank (and vice
versa). Profile cross sections, when present, are flipped with
[`flip_profile()`](https://fluvtools.github.io/xchan/reference/flip_profile.md)
so signed distances across the section stay aligned with the plan. When
there is no profile geometry, only the planimetric geometries are
updated.

If a channel axis is stored
([`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md)),
it is reversed end-for-end with
[`sf::st_reverse()`](https://r-spatial.github.io/sf/reference/geos_unary.html)
so downstream distance
([`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md))
is still measured from the **start** of the axis line — which is now the
opposite physical end of the reach. Cross sections keep the same
projected positions along the same geographic line, but chainage is
recomputed from the new zero at the former downstream end. Then
[`xt_arrange_downstream()`](https://fluvtools.github.io/xchan/reference/xt_arrange_downstream.md)
sorts by increasing chainage along **hydrologic** downstream after the
reversal (for example order D, C, B, A instead of A, B, C, D when those
letters ran upstream-to-downstream before). Section order in the
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) list is
unchanged by this function.

## Note

Summaries that treat left and right symmetrically (e.g.
[`elevation_bank()`](https://fluvtools.github.io/xchan/reference/elevations.md)
with default `min`) are unchanged until you reorder sections (e.g.
[`xt_arrange_downstream()`](https://fluvtools.github.io/xchan/reference/xt_arrange_downstream.md)).

## Examples

``` r
ch <- xt_as_channel(c(10, 12, 11))
ch[1]
#> xchan channel with 1 cross sections.
#> <xsection 1> 10 (-)
xt_reverse_flow(ch)
#> xchan channel with 3 cross sections.
#> <xsection 3> 10 (-)
#> <xsection 2> 12 (-)
#> <xsection 1> 11 (-)
ch[1]
#> xchan channel with 1 cross sections.
#> <xsection 1> 10 (-)
```
