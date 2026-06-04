# Dredge profile cross sections to a target bathymetry

Dredge profile cross sections to a target bathymetry

## Usage

``` r
xt_dredge_to(channel, bathy, ...)

# S3 method for class 'xchan'
xt_dredge_to(channel, bathy, ...)

# S3 method for class 'xsection'
xt_dredge_to(channel, bathy, ...)

# Default S3 method
xt_dredge_to(channel, bathy, ...)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [xsection](https://fluvtools.github.io/xchan/reference/xsection.md)
  object with profile cross sections.

- bathy:

  A bathymetry specification from
  [`bathy_rectangle()`](https://fluvtools.github.io/xchan/reference/bathymetry.md)
  or
  [`bathy_vshape()`](https://fluvtools.github.io/xchan/reference/bathymetry.md).

- ...:

  Reserved for methods (must be empty).

## Value

Object of the same class as `channel`, with profile beds adjusted toward
the target bathymetry.

## Details

Dredging modifies every span between consecutive bank contacts on each
profile cross section to match the supplied bathymetry specification.
Where the existing bed is higher than the target, material is removed;
where the existing channel is deeper than the target, the bed is raised.
Outer bank positions and elevations are unchanged.

Cross sections with mid-channel islands are dredged span by span: each
water interval and each island interior between consecutive bank
contacts receives the same target bathymetry.

This is useful when profile cross sections were sampled from a DEM that
does not represent submerged topography (for example LIDAR): the river
may appear as a flat surface at bank elevation rather than a channel.
Supplying synthetic target bathymetry inserts a channel geometry for
analysis.

## See also

[`bathy_rectangle()`](https://fluvtools.github.io/xchan/reference/bathymetry.md),
[`bathy_vshape()`](https://fluvtools.github.io/xchan/reference/bathymetry.md),
[`elevation_bank()`](https://fluvtools.github.io/xchan/reference/elevations.md)

## Examples

``` r
channel <- xt_as_channel(rep(10, 3))
channel <- xt_add_profile(
  channel,
  distance = distance,
  elevation = elevation,
  section = id,
  banks = is_bank,
  data = profile_survey
)
xt_dredge_to(channel, bathy = bathy_rectangle(depth = 2))
#> xchan channel with 3 cross sections.
#> <xsection 1> 10 m
#> <xsection 2> 12 m
#> <xsection 3> 8 m
#> With profile view
```
