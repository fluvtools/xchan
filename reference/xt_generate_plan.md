# Generate channel object from banklines

Generate a channel object with planimetric cross sections from a single
closed polygon outlining the channel footprint (both banks).

## Usage

``` r
xt_generate_plan(banks, ..., n, spacing, at, axis = NULL, progress = FALSE)
```

## Arguments

- banks:

  Channel footprint as **one closed polygon** (`POLYGON` or
  `MULTIPOLYGON`), typically as
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html),
  [`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html),
  or bare
  [`sf::st_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.html).
  The ring(s) enclose the plan-view channel area; left and right banks
  are **not** supplied as separate inputs—they are inferred from this
  boundary together with the sampling axis (see **Details**).

  **Not supported as `banks`:** two independent open bank polylines
  (e.g. one `LINESTRING` per bank without closing the corridor).
  Automatic axis generation uses
  [`centerline::cnt_path_guess()`](https://centerline.anatolii.nz/reference/cnt_path_guess.html),
  which expects polygon geometry; bank-to-bank segments are found by
  intersecting trial transects with this closed boundary.

- ...:

  Additional arguments (ignored).

- n:

  Number of cross sections to generate (mutually exclusive with spacing
  and at)

- spacing:

  Distance between cross sections (mutually exclusive with `n` and
  `at`). Plain numeric is interpreted in the `banks` CRS length unit; a
  [`units::units()`](https://r-quantities.github.io/units/reference/units.html)
  length object is converted automatically.

- at:

  Specific distances along the channel axis for cross sections (mutually
  exclusive with `n` and `spacing`). Same units treatment as `spacing`.

- axis:

  Channel axis as a multilinestring: the line along which cross sections
  are placed. If `NULL` (the default), an axis is generated
  automatically from the **island-free** footprint
  (`polygon_sans_holes()`) using the **centerline** package
  ([`centerline::cnt_path_guess()`](https://centerline.anatolii.nz/reference/cnt_path_guess.html)).

- progress:

  Logical; if `TRUE`, display a text progress bar while generating
  planimetric cross sections.

## Value

An [`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) with
one
[`xsection`](https://fluvtools.github.io/xchan/reference/xsection.md)
per list position, in downstream order along the sampling axis.
Cross-section identity keys are **not** set; use
[`xt_section_id()`](https://fluvtools.github.io/xchan/reference/xt_section_id.md)
if you need stable keys (for example when joining tabular profiles with
[`xt_add_profile()`](https://fluvtools.github.io/xchan/reference/xt_add_profile.md)).
After subsetting, restore order with
[`xt_arrange_downstream()`](https://fluvtools.github.io/xchan/reference/xt_arrange_downstream.md).
Use
[`xt_distance_downstream()`](https://fluvtools.github.io/xchan/reference/xt_distance_downstream.md)
for distance along the axis from its start to each section (requires the
axis from
[`xt_axis()`](https://fluvtools.github.io/xchan/reference/xt_axis.md),
which this function sets). The sampling axis is stored on the
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md) object,
and the **bank footprint** polygon(s) from `banks` on
[`xt_bankline()`](https://fluvtools.github.io/xchan/reference/xt_bankline.md)
(for plan plotting).

## Details

**Bank geometry:** Supply the channel as one polygon (or multipolygon)
so its boundary is a closed loop around the wetted/plan corridor. If you
only have two bank polylines, convert them to a closed polygon (e.g.
connect upstream/downstream ends) before calling this function.

Interior rings (islands) are dropped with `polygon_sans_holes()` for
axis generation (when `axis` is `NULL`) and for the minimum-width
transect search: each station gets the **shortest bank-to-bank segment**
through that point on the filled corridor, matching the original
algorithm and ignoring islands by construction.

The holed footprint (`banks`) is then used to **refine** that transect:
the filled chord is intersected with the polygon boundary (outer bank
plus island outlines). Distinct intersection points are merged with the
chord endpoints, ordered along the transect, and deduplicated, producing
a plan `LINESTRING` with **two or more** vertices (extra vertices where
the transect meets island banks). Relative distances for profiles still
use the chord from **first to last** vertex
(`transect_xy_from_relative()`).

To define the spacing of the cross sections, a channel axis is first
calculated, and equally spaced points are sampled along that axis. Cross
sections are calculated at these points.

**Downstream** is the direction of increasing distance along that axis
(the same direction used when stations are sorted by
[`sf::st_line_project()`](https://r-spatial.github.io/sf/reference/st_line_project_point.html)).
If you supply `axis`, downstream follows the storage order and
digitization of that line; if the axis is generated automatically,
downstream follows the geometry returned by the centerline routine.

**Left and right bank** mean left and right when standing at the station
on the axis and **facing downstream**, in the map plane of the CRS
(planar coordinates).

**Vertex order (planimetric convention):** each output is a `LINESTRING`
from left bank to right bank: the **first** coordinate is on the left
bank, the **last** on the right bank (with optional intermediate
vertices on island banks). Increasing distance along the polyline (first
to last vertex) matches profile cross sections where distance increases
from left to right along the overall chord.

**Terminal stations:** Cross sections at the upstream and/or downstream
ends of the axis are sometimes awkward (very short transects, odd
intersections with the bank polygon, or mouth artifacts). Inspect the
result and **subset** the returned
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)
(single-bracket indexing; see
[`xchan`](https://fluvtools.github.io/xchan/reference/xchan.md)) to drop
the first and/or last rows if you do not want those sections in later
analysis or plotting.

**How orientation is computed:** for each station we take a unit tangent
to the axis pointing downstream (`axis_unit_tangent_downstream()`). For
each endpoint of the bank-to-bank segment we form the vector from the
station to that endpoint and compute the 2D scalar cross product with
the tangent, \\D_x (E_y - C_y) - D_y (E_x - C_x)\\ where \\(D_x,D_y)\\
is the tangent and \\(C_x,C_y)\\ / \\(E_x,E_y)\\ are the station and an
endpoint. Under the usual planar orientation, the endpoint with the
**larger** value lies on the **left** bank. If that is not already the
first vertex, the segment is reversed with
[`sf::st_reverse()`](https://r-spatial.github.io/sf/reference/geos_unary.html)
(`orient_plan_xs_left_first()`). This does not require splitting the
bank polygon by the axis.

## Examples

``` r
bl <- sf::st_sfc(Squamish_bankline, crs = 3005)
channel <- xt_generate_plan(bl, n = 20)

# With a custom axis (e.g. user-defined line along the channel)
# channel <- xt_generate_plan(bl, n = 20, axis = my_axis)
```
