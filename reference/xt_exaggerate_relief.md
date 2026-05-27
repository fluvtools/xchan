# Exaggerate relief in profile cross sections

Applies a vertical exaggeration factor to profile elevations, measured
as height above each profile's minimum elevation (thalweg baseline). Use
this to improve visual interpretation in profile and 3D views.

## Usage

``` r
xt_exaggerate_relief(x, times = 2, ...)
```

## Arguments

- x:

  An `xs_profile` or
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) object
  with profile geometry.

- times:

  Single non-negative numeric exaggeration factor. Values above 1
  increase vertical relief; values between 0 and 1 compress it.

- ...:

  Must be empty.

## Value

Object of the same class as `x`, with exaggerated profile elevations.

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
profile_object <- channel[[1]]$profile
xt_exaggerate_relief(profile_object)
#> xchan profile cross section
#>   Coordinates: 23 points
#>   Banks: 2 bank points
#>   Thalwegs: 1 thalweg points
#>   Distance range: -15 15 
#>   Elevation range: -1 9.193686 
```
