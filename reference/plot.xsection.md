# Plot one cross section (`xsection`)

Plot the profile view when a profile exists (default), or the
planimetric transect. For profile-only plots you can also call
`plot(x$profile)` on the embedded `xs_profile`.

## Usage

``` r
# S3 method for class 'xsection'
plot(
  x,
  ...,
  view = c("auto", "profile", "plan"),
  extent = c("banks", "full"),
  add = FALSE,
  exaggerate = 1,
  from = NULL,
  to = NULL,
  col = "black",
  lwd = 1,
  banks = c("auto", "show", "hide"),
  col_bank = "deepskyblue3",
  pch_bank = 16,
  cex_bank = 0.65,
  warn_if_no_profile = TRUE
)
```

## Arguments

- x:

  An
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
  object.

- ...:

  Additional arguments passed to
  [`plot.xs_profile`](https://fluvtools.github.io/xchan/reference/plot.xs_profile.md)
  (profile view) or forwarded when plotting the plan view.

- view:

  `"auto"` plots the profile when `x$profile` is present, otherwise the
  plan view. Use `"profile"` or `"plan"` to force one view.

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

- col, lwd, banks, col_bank, pch_bank, cex_bank, warn_if_no_profile:

  Used for the plan view only (see
  [`plot.xchan`](https://fluvtools.github.io/xchan/reference/plot.xchan.md)).

## Value

Called for its graphical side effect.

## Examples

``` r
coords <- matrix(c(-3, 10, 0, 8, 3, 10), ncol = 2, byrow = TRUE)
prof <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
plan_ls <- sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2))
seg <- sf::st_sfc(plan_ls, crs = 3005)
ch <- xchan:::new_channel(seg, profile = list(prof))
plot(ch[[1]])

```
