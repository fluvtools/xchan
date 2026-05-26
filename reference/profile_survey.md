# Toy cross-section survey data

Synthetic tabular profile observations for six cross sections, intended
as example survey input for
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md).
This is **not** data from a real channel.

## Usage

``` r
profile_survey
```

## Format

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 138 rows and 4 columns:

- id:

  Integer cross-section key (`1`–`6`), matching the order of the cross
  sections.

- distance:

  Chord distance along each transect
  ([`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  in metres). Distances increase from left bank toward right bank, with
  negative values on the left floodplain approach and positive values on
  the right.

- elevation:

  Ground elevation at each vertex
  ([`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  in metres).

- is_bank:

  Logical flag; `TRUE` at the left and right bank vertices (distances
  `0` and channel width) for each section.

## Source

Synthetically generated in `data-raw/profile_survey.R`.

## Details

Each section has 23 survey points: ten on the left approach, three along
the channel bottom (including the thalweg), and ten on the right
floodplain. Rows are stored in section order with vertices in survey
chainage order.

## See also

[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md)
