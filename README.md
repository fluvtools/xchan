
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xchan <a href="https://stochagbem.github.io/xchan/"><img src="man/figures/logo.png" align="right" height="139" alt="xchan website" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The purpose of xchan is to create, widen (erode), and calculate
attributes of watercourse geometries. It does this by encoding a channel
by its cross sections, which can be planimetric (line segments arranged
on a map) or profile (slices into the landscape).

The package concept is inspired by the
[**sf**](https://r-spatial.github.io/sf/) package, because xchan encodes
both cross section objects (`xsection`) and whole channels (`xchan`),
which are simply a list of cross sections. Most functions in xchan start
with a common prefix, `xt`, in parallel with the sf package’s function
prefix, `st`. The `x` stands for “cross” or “cross section”.

## Statement of Need

(automation; randomization)

## Target Audience

## Installation

You can install the development version of xchan from
[GitHub](https://github.com/stochaGBEM/xchan) with:

``` r
# install.packages("devtools")
devtools::install_github("stochaGBEM/xchan")
```

## Example

The package includes spatial data for the Squamish River in British
Columbia: a bankline polygon and a DEM. It’s possible to create channels
in other ways; see the [*Getting Started with
Channels*](https://stochagbem.github.io/xchan/articles/basic_channels.html)
and [*Channels with
Profiles*](https://stochagbem.github.io/xchan/articles/channels_with_profiles.html)
vignettes for more information.

Start by loading the package and unwrapping the packaged DEM, and
plotting the bankline polygon on top.

``` r
library(xchan)
library(terra)
#> terra 1.9.27
#> 
#> Attaching package: 'terra'
#> The following objects are masked from 'package:testthat':
#> 
#>     compare, describe
dem <- unwrap(demo_dem)
plot(dem)
plot(demo_bankline, add = TRUE, col = "lightblue")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" alt="" width="100%" />

Generate planimetric cross sections from the bankline polygon, spaced
apart by 100 meters.

``` r
squamish <- xt_generate_plan(demo_bankline, spacing = 100)
plot(squamish)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

If your workflow requires profile cross sections, you can generate them
by sampling the DEM at each planimetric cross section.

``` r
squamish <- xt_generate_profile(squamish, unwrap(demo_dem), sample_freq = 2)
print(squamish, n = 10)
#> xchan channel with 210 cross sections.
#> CRS: EPSG:3005 
#> <xsection 1> 73.43041 m
#> <xsection 2> 164.4219 m
#> <xsection 3> 240.8035 m
#> <xsection 4> 180.8825 m
#> <xsection 5> 224.0966 m
#> <xsection 6> 220.0995 m
#> <xsection 7> 217.2965 m
#> <xsection 8> 167.5596 m
#> <xsection 9> 151.5943 m
#> <xsection 10> 142.1572 m
#> ... 200 more cross sections
#> With profile view
```

Under the hood, a channel is just a list, as hinted at with the
`print()` output.

Including profile cross sections also has the effect of extending the
cross sections beyond the banks. Here is what the 10th cross section
looks like in its full extent, with an exaggeration factor of 1.5.

``` r
plot(squamish[[10]], extent = "full", exaggerate = 1.5)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="100%" />

Widen the channel by 20 meters on the right bank.

``` r
widened_squamish <- xt_widen(squamish, dw = 20, side = "right")
plot(widened_squamish)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="" width="100%" />

Take a look at the 10th profile cross section now:

``` r
plot(widened_squamish[[10]], extent = "full", exaggerate = 1.5)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" alt="" width="100%" />

Calculate the new channel widths.

``` r
head(xt_width(widened_squamish))
#> Units: [m]
#> [1]  93.43041 184.42194 260.80346 200.88251 244.09664 240.09946
```

Calculate the channel gradient, using the lower bank as the reference
elevation.

``` r
grad <- xt_gradient(widened_squamish, elevation = elevation_bank(min))
head(grad)
#> [1]           NA  0.003343378  0.005047202  0.002018437  0.007469054
#> [6] -0.006375794
```

Plot the gradient along the channel axis.

``` r
dist <- xt_distance_downstream(widened_squamish)
plot(dist, grad)
lines(dist, grad, type = "l")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" alt="" width="100%" />

To learn more about channel computations like these, see the [*Channel
Computations*](https://stochagbem.github.io/xchan/articles/channel_computations.html)
vignette.

## xchan in the Context of Other R Packages

The following packages are relevant for channel geometries in R.

- The [**centerline**](https://cran.r-project.org/package=centerline)
  package is used to generate a centerline from a bankline polygon.
- The [**sf**](https://r-spatial.github.io/sf/) package is used to
  manipulate vector-based spatial objects.
- The [**terra**](https://rspatial.org/terra/) package is used to
  manipulate both vector and raster-based spatial objects.

## Attribution

The creation of this package would not have been possible without
funding from [BGC Engineering Inc.](https://bgcengineering.ca/).
