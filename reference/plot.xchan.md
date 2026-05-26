# Plot a channel object

Plot a channel object

## Usage

``` r
# S3 method for class 'xchan'
plot(
  x,
  ...,
  extent = c("banks", "full"),
  axis = c("line", "arrows", "none"),
  banks = c("auto", "show", "hide"),
  add = FALSE
)
```

## Arguments

- x:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  object.

- ...:

  Additional arguments passed to
  [`plot()`](https://rspatial.github.io/terra/reference/plot.html) for
  plan geometries and to bank-marker styling (`col_bank`, `pch_bank`,
  `cex_bank`).

- extent:

  One of `"banks"` (default) or `"full"`. `"full"` draws each transect
  out to the ends of its profile (when profiles are present); uses
  bank-to-bank segments only if there is no profile geometry (with a
  warning). Profile bank markers are drawn only when `extent = "full"`
  and bank markers are enabled (see `banks`).

- axis:

  How to draw the stored channel axis
  ([`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md)),
  when one is present: `"line"` (default) draws the axis as a plain
  line, `"arrows"` draws flow direction along the axis, `"none"` omits
  it.

  When
  [`xt_bankline()`](https://fluvtools.github.io/xchan/reference/xt_bankline.md)
  is set and `add = FALSE`, the footprint is drawn first (filled polygon
  under transects), then cross sections and the axis overlay.

- banks:

  One of `"auto"`, `"show"`, or `"hide"`: whether to draw bank markers
  on planimetric transects (endpoints, and all profile banks when
  `extent = "full"`). `"auto"` draws markers when there is no
  [`xt_bankline()`](https://fluvtools.github.io/xchan/reference/xt_bankline.md)
  footprint, and omits them when a footprint is present (markers are
  redundant with the polygon boundary). `"show"` / `"hide"` override
  that rule.

- add:

  If `TRUE`, draw on the current plot (same rules as
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html)).

## Details

For the plan view, when `add = FALSE` and you do not pass `xlim` /
`ylim`, the plot limits are adjusted so the on-screen aspect is not
extremely stretched (for example when synthetic width-only channels use
a large default station spacing relative to transect width). Pass your
own `xlim` and `ylim` to reproduce the raw map scale.

For "profile" and "3d" views, you can specify `exaggerate` to vertically
exaggerate the relief. For example, `exaggerate = 2` doubles the
vertical scale. The default is `exaggerate = 1` (no exaggeration). It is
strongly recommended not going beyond 3, because exaggeration beyond
this point can distort the perception of the profile.
