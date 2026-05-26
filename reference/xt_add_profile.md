# Attach or replace profile cross sections from tabular data

Build `xs_profile` objects from chord-distance / elevation columns (and
bank flags), attach them to each
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
and reconcile plan vs profile bank-to-bank width using the same checks
as [`xt_width()`](https://fluvtools.github.io/xchan/reference/widths.md)
on plan and profile. Distances are **straight chord stationing** along
the bank-to-bank transect (not arc length along a meandering ground
path).

## Usage

``` r
xt_add_profile(channel, ...)

# S3 method for class 'xchan'
xt_add_profile(
  channel,
  distance,
  elevation,
  section,
  banks,
  ...,
  data = NULL,
  snap_banks_to = c("profile", "plan")
)

# S3 method for class 'xsection'
xt_add_profile(
  channel,
  distance,
  elevation,
  banks,
  ...,
  data = NULL,
  snap_banks_to = c("profile", "plan")
)
```

## Arguments

- channel:

  An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md).

- ...:

  Must be empty. Arguments after `...` (`data`, `snap_banks_to`) must be
  matched by name.

- distance, elevation:

  Tidy-eval expressions (see `data`) for chord distance and elevation;
  may be plain numeric or
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  (distance is converted with the channel CRS length unit when
  available). Distances are expected to increase **from left bank toward
  right bank** along each transect (the same sense as planimetric chord
  stationing). If your survey chainage ran the opposite way, flip or
  negate the distance column before calling this function.

- section:

  Tidy-eval expression giving a **stable cross-section key** for each
  row (integer, character, etc.). Values are matched to
  [`xt_section_id()`](https://fluvtools.github.io/xchan/reference/xt_section_id.md)
  when that vector exists and has length `length(channel)`; otherwise to
  `seq_len(length(channel))`. Larger keys do **not** imply downstream
  order here — only identity. Rows are taken in **data frame row order**
  within each section (no sorting is applied by this function).

- banks:

  Tidy-eval logical vector: `TRUE` at bank vertices (even count per
  section). The **outer** banks are those with minimum and maximum chord
  distance among `TRUE` rows; inner `TRUE` values encode islands.

- data:

  For [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
  and
  [`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md),
  `NULL` (the default) evaluates `distance`, `elevation`, `section` (for
  [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) only),
  and `banks` in the calling environment. Otherwise a data frame (or
  data-mask object) whose rows align with those vectors.

- snap_banks_to:

  If `"plan"`, outer bank **distances** in the profile are set to **plus
  or minus** half the planimetric segment length (inner bank distances
  are unchanged; an error is raised if an inner bank would lie outside
  that span). If `"profile"`, plan segments are widened or narrowed with
  [`xt_widen()`](https://fluvtools.github.io/xchan/reference/xt_widen.md)
  so their length matches the surveyed outer bank span (symmetric
  split). There is no affine stretch of interior profile distances.

## Value

A **new**
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) or
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
with profiles attached or replaced (the input object is not modified).
For [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md),
**every** section receives a new profile in one call (required for
profile homogeneity).

## Details

Each built `xs_profile` stores chord distances with **0 at the midpoint
between the two outer bank distances**, vertices sorted by increasing
distance (stable ordering for ties), and thalwegs re-derived from the
sorted polyline. Existing profiles on a channel are always **replaced**
in full. If the channel already had profiles, supply rows for **all**
sections identified by
[`xt_section_id()`](https://fluvtools.github.io/xchan/reference/xt_section_id.md)
(or `seq_len(n)` when that vector is absent).
