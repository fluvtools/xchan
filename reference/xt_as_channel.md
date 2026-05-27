# Coerce to a channel object (`xchan`)

Convert widths, line geometries, a list of
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
objects, or an existing
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) into
cross-section geometry. Width, `sfc`, and `list` methods return an
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md).

## Usage

``` r
xt_as_channel(x, ...)

# S3 method for class 'numeric'
xt_as_channel(x, ..., crs = NULL, axis = NULL, bankline = NULL)

# S3 method for class 'units'
xt_as_channel(x, ..., crs = NULL, axis = NULL, bankline = NULL)

# S3 method for class 'sfg'
xt_as_channel(x, ..., crs = NULL, axis = NULL, bankline = NULL)

# S3 method for class 'sfc'
xt_as_channel(x, ..., crs = NULL, axis = NULL, bankline = NULL)

# S3 method for class 'list'
xt_as_channel(x, ..., crs = NULL, axis = NULL, bankline = NULL)

# S3 method for class 'xchan'
xt_as_channel(x, ..., crs = NULL, axis = NULL, bankline = NULL)

# Default S3 method
xt_as_channel(x, ...)
```

## Arguments

- x:

  Object to coerce (`numeric` vector of widths, `sfc`, `list` of
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
  or existing
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)).

- ...:

  Must be empty except where documented below.

- crs:

  For `numeric`, `sfc`, and `list` methods: CRS applied to plan
  geometries via
  [`sf::st_set_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).
  `NULL` leaves existing CRS unchanged (for `list`, sets the container
  CRS on the
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)).

- axis:

  Optional channel axis (`sfc`/`sfg` LINESTRING, length 1); see
  [`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md).
  When `NULL`, a default axis is built (synthetic spacing for `numeric`
  widths; midpoints connected in section order for `sfc`, `sfg`, and
  `list`). For an existing
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md),
  `NULL` leaves the stored axis unchanged.

- bankline:

  Optional bankline polygon (`sfc`/`sfg`); stored on the
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) via
  [`xt_bankline()`](https://fluvtools.github.io/xchan/reference/xt_bankline.md).
  `NULL` leaves any existing footprint unchanged.

## Value

An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) for
`numeric`, `units`, `sfc`, `sfg`, `list`, and `xchan` methods.

## Details

For **`sfg`** / **`sfc`** inputs, coercion targets **planimetric-only**
cross sections at this stage of package development: geometries are cast
to **LINESTRING** bank-to-bank segments. Users should supply line
geometries (not polygons or points). Profile views must be attached
separately (for example with
[`xchan()`](https://fluvtools.github.io/xchan/reference/xchan.md) /
[`xsection()`](https://fluvtools.github.io/xchan/reference/xsection.md)).

When coercing **`numeric`** widths without an explicit `axis`, cross
sections are placed on **vertical** transects (constant \\x\\, width in
\\y\\) so the synthetic channel runs **horizontally** along \\x\\. Each
transect’s first vertex is the **left** bank and the second the
**right** bank, facing downstream (increasing \\x\\ along the default
axis). Consecutive stations are spaced by **twice** a reference width:
twice [`stats::median()`](https://rdrr.io/r/stats/median.html) of `x`
when that value is positive, otherwise twice `mean(x)`. If every width
is zero, a reference width of `1` is used (so consecutive stations lie 2
map units apart). The same length unit applies as for `x` (typically
metres under a projected CRS).

## See also

[`xchan()`](https://fluvtools.github.io/xchan/reference/xchan.md),
[`xsection()`](https://fluvtools.github.io/xchan/reference/xsection.md)

## Examples

``` r
# Synthetic widths (stations spaced ~2 median widths along x by default)
xt_as_channel(c(10, 15, 12, 8))
#> xchan channel with 4 cross sections.
#> <xsection 1> 10 (-)
#> <xsection 2> 15 (-)
#> <xsection 3> 12 (-)
#> <xsection 4> 8 (-)

library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
seg <- st_sfc(
  st_linestring(matrix(c(-0.2, 0.3, 0.2, 1), nrow = 2, byrow = TRUE)),
  st_linestring(matrix(c(0.1, 0.1, 1, 1), nrow = 2, byrow = TRUE)),
  crs = 3005
)
xt_as_channel(seg)
#> xchan channel with 2 cross sections.
#> CRS: EPSG:3005 
#> <xsection 1> 0.8062258 m
#> <xsection 2> 1.272792 m
```
