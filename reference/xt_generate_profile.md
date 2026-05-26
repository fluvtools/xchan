# Sample DEM to generate profile cross sections

Generate profile cross sections for a channel object using a digital
elevation model (DEM). This function samples the DEM along the
planimetric cross sections and creates xs_profile objects for each cross
section.

## Usage

``` r
xt_generate_profile(
  channel,
  dem,
  ...,
  extent_distance = Inf,
  extent_multiplier = NULL,
  sample_freq,
  sample_n,
  progress = FALSE
)
```

## Arguments

- channel:

  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) with
  planimetric cross sections.

- dem:

  Digital elevation model (raster or terra object).

- ...:

  Must be empty.

- extent_distance:

  Distance to extend beyond banks on each side. Use `Inf` (the default)
  to extend along the cross section until the DEM bounding box is
  reached in each direction. A finite plain numeric is interpreted in
  the channel's CRS length unit; a
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  length object is converted automatically. If larger than the distance
  from each bank to the DEM edge along the cross-section line, the
  extension is shortened silently on that side. Mutually exclusive with
  passing a non-`NULL` `extent_multiplier`.

- extent_multiplier:

  Multiplier of channel width to extend beyond banks; positive numeric
  value. Default `NULL` uses `extent_distance` instead. Mutually
  exclusive with supplying an explicit `extent_distance` argument.

- sample_freq:

  Distance between DEM sampling points; positive value. Same units
  treatment as `extent_distance`. Mutually exclusive with `sample_n`.

- sample_n:

  number to sample; positive integer value greater than 1. Mutually
  exclusive with `sample_freq`.

- progress:

  If `TRUE`, show a text progress bar while processing cross sections
  (same behaviour as
  [`xt_generate_plan()`](https://fluvtools.github.io/xchan/reference/xt_generate_plan.md)).

## Value

Updated [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
with profile geometry attached to each cross section.

## Details

This function extends the planimetric cross sections beyond the banks to
create a "frame" for erosion analysis. The extent can be specified
either as a fixed distance or as a multiplier of the channel width.
Similarly, the sampling distance can be specified either as a fixed
distance or as a multiplier of the channel width.

Extension beyond banks is clipped to the DEM bounding box and, when
`extent_distance` is infinite, to the nearest valid-data edge along each
bank ray (in the DEM's CRS, projected to the cross-section CRS when
needed). Sampling therefore stays inside contiguous valid cells when the
bank-to-bank segment lies inside the DEM.

Elevations are taken with bilinear interpolation between cell centres
(see
[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
with `method = "bilinear"`), which smooths grid-oriented steps relative
to nearest-neighbour extraction but still respects the DEM cell size:
expect subtle terrace-like breaks where slope crosses grid boundaries if
resolution is coarse.

Sample points that fall in nodata cells (`NA`) at the **ends** of the
transect are dropped until all remaining elevations are finite; interior
gaps still raise an error. Any sample outside the raster bounding box
raises an error.

## Examples

``` r
# Sample DEM with 50 m extension beyond banks and 1 m sample spacing
# channel_with_profiles <- xt_generate_profile(
#   channel, dem, extent_distance = 50, sample_freq = 1
# )

# 2x channel width extension; sample count sets spacing along extended line
# channel_with_profiles <- xt_generate_profile(
#   channel, dem, extent_multiplier = 2, sample_n = 100
# )
```
