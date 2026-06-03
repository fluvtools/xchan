# Bathymetry specifications for dredging profile cross sections

These functions construct **bathymetry specification** objects (class
`"xchan_bathymetry"`). Pass one to
[`xt_dredge_to()`](https://fluvtools.github.io/xchan/reference/xt_dredge_to.md)
to replace the wetted bed of each profile cross section with a synthetic
channel shape.

## Usage

``` r
bathy_rectangle(depth, wse = elevation_bank())

bathy_vshape(depth, thalweg_frac, wse = elevation_bank())
```

## Arguments

- depth:

  Channel depth below `wse`. Must be non-negative.

- wse:

  Water surface elevation (WSE) specification (class
  `"xchan_elevation"`).

- thalweg_frac:

  Position of the thalweg as a fraction of the distance from the left
  bank to the right bank within each dredged span (`0` = left bank, `1`
  = right bank). Values of `0` or `1` place the full channel depth at
  that bank with a vertical bed cliff there.

## Value

An object inheriting `"xchan_bathymetry"`: a small bathymetry
specification consumed by
[`xt_dredge_to()`](https://fluvtools.github.io/xchan/reference/xt_dredge_to.md).

## Functions

- `bathy_vshape()`: V-shaped channel with zero depth at the banks and
  `depth` below `wse` at the thalweg. Use `thalweg_frac = 0` or `1` for
  a vertical bed cliff at the left or right bank.

## Water surface and depth

Both constructors take a `depth` measured downward from a water-surface
elevation (`wse`, water surface elevation). By default, `wse` is
[`elevation_bank()`](https://fluvtools.github.io/xchan/reference/elevations.md),
which uses the lower of the two outer bank elevations on each cross
section.

## Dredging vs filling

Dredging adjusts profile geometry toward the target bathymetry. Material
is removed where the existing bed lies above the target, and added where
the existing channel is deeper than the target. Bank positions and
elevations are preserved; spans between consecutive bank contacts are
modified.

## Islands and multi-channel profiles

When a cross section has more than one wetted interval (for example a
mid-channel island), every span between consecutive bank contacts is
dredged independently, including the island interior between inner bank
pairs. The same target bathymetry is applied to each span on that
section.

## See also

[`xt_dredge_to()`](https://fluvtools.github.io/xchan/reference/xt_dredge_to.md),
[`elevation_bank()`](https://fluvtools.github.io/xchan/reference/elevations.md)
