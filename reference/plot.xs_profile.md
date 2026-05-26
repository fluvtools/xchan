# Plot profile cross section

Plot a profile cross section showing the elevation profile. This
function is primarily for internal use or advanced users. Most users
should work with channel objects using plot_plan() or plot_3d().

## Usage

``` r
# S3 method for class 'xs_profile'
plot(
  x,
  ...,
  extent = c("banks", "full"),
  add = FALSE,
  exaggerate = 1,
  from = NULL,
  to = NULL
)
```

## Arguments

- x:

  An xs_profile object

- ...:

  Additional arguments passed to plot

- extent:

  Selects the default horizontal span: `"banks"` uses the span between
  the outer bank distances; `"full"` uses the full range of profile
  sample distances (default `"banks"`).

- add:

  Logical. Add to existing plot?

- exaggerate:

  Positive numeric. **Physical** vertical exaggeration: passed as `asp`
  to
  [graphics::plot.default](https://rdrr.io/r/graphics/plot.default.html),
  so one data unit along **y** is drawn with the same length as
  `exaggerate` data units along **x** (default `1` gives true 1:1
  scaling on the device). Values above 1 stretch the profile vertically;
  values between 0 and 1 compress it. Ignored when `add = TRUE`.

- from, to:

  Optional adjustments to that window (distance along the profile). The
  plotted range starts at `from` when given, otherwise at the default
  left edge for `extent`; it ends at `to` when given, otherwise at the
  default right edge. You may pass `from` only, `to` only, both, or
  neither. Values may lie outside the data range (empty band on that
  side).

## Value

A plot of the profile cross section

## Details

Elevation values are not rescaled; only the plot aspect ratio changes.
For actually modifying stored elevations, use
[`xt_exaggerate_relief()`](https://fluvtools.github.io/xchan/reference/xt_exaggerate_relief.md).

Finite `asp` (from `exaggerate`) is implemented by
[graphics::plot.window](https://rdrr.io/r/graphics/plot.window.html),
which **may expand** `xlim` / `ylim` so the aspect ratio can be
satisfied in the physical plotting region. That is strongest for very
large `exaggerate`; see
[`?plot.window`](https://rdrr.io/r/graphics/plot.window.html) (`asp`).
It is not mis-handling of `from` / `to` in this package.

For the horizontal axis, `extent` fixes the reference span
(`rng_default`). Each of `from` and `to` overrides one endpoint only
when supplied; omitted endpoints use `rng_default`.

## Examples

``` r
# Plot a profile cross section (advanced use)
plot(profile_object)
#> Error: object 'profile_object' not found

# Plot with vertical exaggeration
plot(profile_object, exaggerate = 2)
#> Error: object 'profile_object' not found

# Narrow or shift the window relative to `extent`
plot(profile_object, extent = "full", from = -100)
#> Error: object 'profile_object' not found
plot(profile_object, extent = "banks", to = 50)
#> Error: object 'profile_object' not found
```
