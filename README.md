---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# xchan: simple channel cross sections

<!-- badges: start -->
[![R-CMD-check](https://github.com/stochaGBEM/xchan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stochaGBEM/xchan/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/stochaGBEM/xchan/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stochaGBEM/xchan?branch=main)
<!-- badges: end -->

The purpose of xchan is to create and manipulate watercourse geometries, with a focus on cross sections. Because this package sits on top of the sf package, geometries can also be manipulated in the same way as in the sf package.

The name of the package is inspired by the sf package. Functions in xchan start with a common prefix, `xt`, which stands for "cross-section type"; this is intended to parallel the sf package's function prefix, `st`, which stands for "spatial type".

**Data model.** The main reach-scale object is [`xchan`]: a list of [`xsection`]
objects with CRS (and optionally a channel axis; see [`xt_axis()`]) stored as
attributes on the container. Single-bracket subsetting `[` keeps those
attributes; use `[[i]]` for one section. If you need tabular metadata (reach
IDs, roughness, …), keep it in your own data frame and hold geometry in an
`xchan` column or alongside it—this package does not attach a special
data-frame class to channels.

## Installation

You can install the development version of xchan from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stochaGBEM/xchan")
```

## Example

The package includes Fraser River demo data that can be used to build a
typical cross-section workflow. Start by loading the package and
unwrapping the packaged DEM:


``` r
library(xchan)

# Unwrap the packaged DEM to a `terra::SpatRaster` object for use in the package.
dem <- terra::unwrap(fraser_dem)
```

Generate planimetric cross sections from the bankline polygon and trace
a channel axis along the cross sections:


``` r
planimetric_cross_sections <- xt_generate_plan(fraser_bankline, spacing = 200)
centerline <- xt_trace_centerline(planimetric_cross_sections)

plot(fraser_bankline, col = "grey90", border = "grey50")
plot(planimetric_cross_sections, add = TRUE, col = "dodgerblue3")
plot(centerline, add = TRUE, col = "cadetblue1", lwd = 2)
```

<div class="figure">
<img src="man/figures/README-unnamed-chunk-3-1.png" alt="plot of chunk unnamed-chunk-3" width="100%" />
<p class="caption">plot of chunk unnamed-chunk-3</p>
</div>

Sample the DEM to create profile cross sections:


``` r
profile_cross_sections <- xt_generate_profile(
  planimetric_cross_sections,
  dem,
  extent_distance = 200,
  sample_n = 151
)

plot(fraser_bankline, col = "grey90", border = "grey50")
plot(profile_cross_sections, add = TRUE, col = "coral")
```

<div class="figure">
<img src="man/figures/README-unnamed-chunk-4-1.png" alt="plot of chunk unnamed-chunk-4" width="100%" />
<p class="caption">plot of chunk unnamed-chunk-4</p>
</div>

Profile cross sections can also be generated as distance-elevation
objects. Each profile cross section contains a distance-elevation geometry that
can be inspected directly:

``` r
# Profile cross sections (distance-elevation):
channel <- xt_generate_profile(
  channel = planimetric_cross_sections,
  dem = dem,
  extent_distance = 200,
  sample_n = 151
)

# Plot an example profile cross section
plot(xt_profile_at(profile_cross_sections, 1))
```

<div class="figure">
<img src="man/figures/README-unnamed-chunk-5-1.png" alt="plot of chunk unnamed-chunk-5" width="100%" />
<p class="caption">plot of chunk unnamed-chunk-5</p>
</div>

Apply a widening scenario to the generated cross sections. In this
example, the channel is widened by 20 metres on the right bank:


``` r
widened_cross_sections <- xt_widen(
  planimetric_cross_sections,
  dw = 200,
  side = "right"
)

plot(fraser_bankline)
plot(widened_cross_sections, add = TRUE)
```

<div class="figure">
<img src="man/figures/README-unnamed-chunk-6-1.png" alt="plot of chunk unnamed-chunk-6" width="100%" />
<p class="caption">plot of chunk unnamed-chunk-6</p>
</div>
