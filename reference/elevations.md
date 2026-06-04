# Elevation specifications for channel profiles

These functions construct **elevation specification** objects (class
`"xchan_elevation"`). An elevation specification is a small callable
wrapper: passing it as `reference` to
[`xt_elevation()`](https://fluvtools.github.io/xchan/reference/xt_elevation.md)
runs it against the channel and returns one elevation per cross section
(aligned with `[[i]]` storage order of the
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)).

## Usage

``` r
elevation_thalweg()

elevation_bank(.f = min, ...)

elevation_bank_left()

elevation_bank_right()

elevation_bed(.f = mean, ...)
```

## Arguments

- .f:

  Numeric summary function applied per cross section. Each call uses a
  single numeric vector `x` as the first argument to `.f` (not separate
  left/right arguments); further arguments are forwarded from `...`: \*
  `elevation_bank()` — `x` has length 2: outer **left** then **right**
  bank elevations (`c(z_left, z_right)`). Default base::min returns the
  lower bank elevation; [base::mean](https://rdrr.io/r/base/mean.html)
  averages the two banks. \* `elevation_bed()` — `x` is the vector of
  elevation (`z`) values at **encoded** profile vertices on the **wetted
  bed** (within each water interval between banks; islands excluded).
  Default [base::mean](https://rdrr.io/r/base/mean.html) is an
  unweighted mean of those values.

- ...:

  Further arguments forwarded to `.f` (for example `probs` for
  [stats::quantile](https://rdrr.io/r/stats/quantile.html)).

## Value

An object inheriting `"xchan_elevation"`: a function `(channel)` that
returns a numeric vector of elevations, one per cross section in storage
order (`[[i]]` of `channel` when it is an
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)). Also
used as `reference` in other package functions expecting an elevation
specification.

## Details

`elevation_thalweg()` takes no arguments: each section contributes its
stored `thalweg_elev` value — the minimum elevation among wetted profile
vertices — documented as a return component of each `xs_profile`.

## Functions

- `elevation_bank()`: Banks: apply `.f(x, ...)` where `x` is
  `c(z_left, z_right)` at the **outer left** and **outer right** bank
  vertices (same points as `elevation_bank_left()` /
  `elevation_bank_right()`). Default `.f = min` returns the lower bank
  elevation.

- `elevation_bank_left()`: Elevation (`z`) at the **outer left bank**
  profile vertex only.

- `elevation_bank_right()`: Elevation (`z`) at the **outer right bank**
  profile vertex only.

- `elevation_bed()`: Apply `.f(x, ...)` to encoded vertex elevations `x`
  on the **wetted bed** (within each water interval between banks; dry
  islands excluded). Vertices are included or excluded by distance along
  the profile only; `.f` does not interpolate or integrate along the
  bed. Default `.f = mean` is an unweighted mean of those `z` values.

## Profile geometry and naming

Objects of class `xs_profile` hold a matrix `coordinates` with column 1
= **distance along the cross section** (left bank toward right bank,
increasing) and column 2 = **elevation**. **Bank** points are vertices
on that polyline flagged in `banks` (row indices); **thalweg**
identifies the deepest part of the traced section.

In this vertical slice, **left** and **right** mean smaller vs larger
distance along the profile (typically left and right banks in map view
when the section is oriented consistently). `elevation_bed()` summarizes
elevations at **stored profile vertices** whose distance lies on the
**wetted bed**: inside each **water** interval between consecutive banks
(alternating water / land / water along the section; see
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md)
and `xs_profile` structure). Dry islands and floodplain tails outside
the outer banks are excluded. The summary is applied to those vertex
elevations only — there is no interpolation along the bed, no distance
weighting, and no integration over width; default
[base::mean](https://rdrr.io/r/base/mean.html) is an unweighted
arithmetic mean of the encoded `z` values (sampling density along the
profile therefore affects it).

## See also

[`xt_elevation()`](https://fluvtools.github.io/xchan/reference/xt_elevation.md),
[`xt_gradient()`](https://fluvtools.github.io/xchan/reference/xt_gradient.md),
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md)

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
xt_elevation(channel, reference = elevation_thalweg())
#> [1] -1 -1 -3 -2 -2 -3
xt_elevation(channel, reference = elevation_bank())
#> [1] 0 0 0 0 0 0
xt_elevation(channel, reference = elevation_bank_left())
#> [1] 0.5 0.5 0.5 0.5 0.5 0.5
xt_elevation(channel, reference = elevation_bank_right())
#> [1] 0 0 0 0 0 0
xt_elevation(channel, reference = elevation_bed())
#> [1] -0.1666667 -0.1666667 -0.8333333 -0.5000000 -0.5000000 -0.8333333
elevation_thalweg()
#> Elevation: thalweg 
#> Parameters: none
elevation_bank()
#> Elevation: bank 
#> Parameters:
#>   .f = .Primitive("min")
elevation_bank_left()
#> Elevation: bank_left 
#> Parameters: none
elevation_bank_right()
#> Elevation: bank_right 
#> Parameters: none
elevation_bed()
#> Elevation: bed 
#> Parameters:
#>   .f = function (x, ...) UseMethod("mean")
```
