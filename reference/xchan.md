# Construct a vector of cross sections (`xchan`)

`xchan` is a list-like geometry container, analogous to an `sfc` object
in **sf**. CRS (and optionally the channel axis; see
[`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md))
are stored as attributes on the container, not repeated on each section.

## Usage

``` r
xchan(sections, crs = NULL, axis = NULL)

# S3 method for class 'xchan'
x[i, ...]
```

## Arguments

- sections:

  A list of `xsection` objects.

- crs:

  Optional CRS accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

- axis:

  Optional reach-scale axis as a single `LINESTRING` (`sfc` or `sfg`),
  same CRS as the plan geometry; see
  [`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md).

- x:

  An `xchan` object.

- i:

  Indices selecting cross sections by position.

- ...:

  Must be empty.

## Value

An object of class `"xchan"` / `"xchan_geom"`.

## Details

The internal layout is deliberately exposed: each cross section is one
element of the list (`[[i]]` is an
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)).
You can inspect or replace sections directly, and combine them with
ordinary list tools. Single-bracket subsetting (`[`) preserves
**`crs`**, **`axis`**, **`bankline`**, and **`section_i`** (parent list
positions used to build the subset; query or replace keys with
[`xt_section_id()`](https://fluvtools.github.io/xchan/reference/xt_section_id.md)).
Double-bracket (`[[`) returns a bare
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md) by
design.

## Methods (by generic)

- `[`: Subset by section index; preserves `crs`, `axis`, `bankline`, and
  `section_i` (parent list positions; see
  [`xt_section_id`](https://fluvtools.github.io/xchan/reference/xt_section_id.md)).
