# Convert channel geometry to `sfc`

Returns simple feature geometry columns for plan, profile, or 3D views
of an [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md).
Combine with
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
yourself if you need a data frame of attributes alongside geometry.

## Usage

``` r
xt_as_sfc(
  channel,
  ...,
  what = c("plan", "profile", "3d"),
  extent = c("banks", "full", "wetted")
)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  object.

- ...:

  Must be empty.

- what:

  Type of spatial object to return: `"plan"` for map-view cross sections
  (default); `"profile"` for each profile as a `LINESTRING` in
  distance–elevation space (no CRS); `"3d"` for 3D multilinestrings
  built by mapping profile elevations onto plan coordinates (same CRS as
  the plan).

- extent:

  `"banks"` restricts geometry to the bank-to-bank span in each
  representation; `"full"` uses the full sampled profile span (and for
  `what = "plan"`, map segments spanning each profile's horizontal
  range); `"wetted"` keeps only **water** intervals between consecutive
  bank contacts (dry islands excluded), so each cross section may be a
  `MULTILINESTRING` when an island splits the channel. For
  `what = "plan"` with `"full"`, if there is no profile view a warning
  is issued and bank-to-bank geometries are returned instead. For
  `"wetted"` without profiles, water intervals are taken from plan
  vertices (even count, alternating water / land / water).

## Value

An `"sfc"` object.

## Examples

``` r
ch <- xt_as_channel(c(2, 2), crs = 3005)
plan_view <- xt_as_sfc(ch, what = "plan")
plan_view
#> Geometry set for 2 features 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: -1 xmax: 4 ymax: 1
#> Projected CRS: NAD83 / BC Albers
#> LINESTRING (0 1, 0 -1)
#> LINESTRING (4 1, 4 -1)
plot(xt_as_sfc(ch, what = "plan"))
```
