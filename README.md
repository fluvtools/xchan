
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xchan <a href="https://stochagbem.github.io/xchan/"><img src="man/figures/logo.png" align="right" height="139" alt="xchan website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/xchan)](https://CRAN.R-project.org/package=xchan)
<!-- badges: end -->

The purpose of xchan is to create, widen (erode), and calculate
attributes of watercourse geometries. It does this by encoding a channel
by its cross sections, which can be planimetric (line segments arranged
on a map) or profile (slices into the landscape).

The package concept is inspired by the [**sf**]() package, because xchan
encodes both cross section objects (`xsection`) and whole channels
(`xchan`), which are simply a list of cross sections. Most functions in
xchan start with a common prefix, `xt`, in parallel with the sf
package’s function prefix, `st`. The `x` stands for “cross” or “cross
section”.

## Statement of Need

(automation; randomization)

## Target Audience

## Installation

You can install the development version of xchan from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stochaGBEM/xchan")
```

## Example

The package includes spatial data for the Squamish River in British
Columbia: a bankline polygon and a DEM. It’s possible to create channels
in other ways; see the [*Getting Started with Channels*]() and
[*Channels with Profiles*]() vignettes for more information.

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
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
#> Warning: River has eroded into a part of the floodplain that's lower in
#> elevation than the thalweg. The original thalweg is still being interpreted as
#> the thalweg.
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
xt_width(widened_squamish)
#> Units: [m]
#>   [1]  93.43041 184.42194 260.80346 200.88251 244.09664 240.09946 237.29650
#>   [8] 187.55961 171.59433 162.15724 147.89787 179.45508 158.77414 144.13340
#>  [15] 135.32378 130.99366 135.52576 148.26494 168.18838 181.97554 185.35597
#>  [22] 190.75386 183.82381 127.95672 160.75844 202.15311 223.35955 231.41809
#>  [29] 225.60719 218.86675 211.38615 164.35550 160.16899 246.16583 280.07985
#>  [36] 292.27021 281.47831 241.81818 269.96797 357.25418 338.80057 294.77830
#>  [43] 312.85012 161.50859 126.20875 186.23011 265.24047 299.88738 316.61599
#>  [50] 493.60492 636.62471 744.17434 710.30622 511.32593 371.47288 259.35425
#>  [57] 167.20780 109.21310  90.88817 113.96811 155.57837 158.69633 154.04836
#>  [64] 164.46052 181.93777 169.02330 145.65448 117.65526 125.83240 140.59145
#>  [71] 125.09071 118.59876 122.37944 116.20063 118.27877 116.22531 113.42236
#>  [78] 106.41516 122.84634 127.11586 122.77688 131.67889 132.53521 130.67450
#>  [85] 139.54645 150.97335 138.27839 131.57561 101.33454 114.93370 143.07453
#>  [92] 138.00485 145.22423 127.82636 124.21868 121.99292 123.55714 121.71536
#>  [99] 110.13979 113.13592 116.66653 115.63450 122.67569 109.39985  97.85220
#> [106] 111.28276 133.39558 146.22842 165.55522 185.06969 173.92347 172.46251
#> [113] 174.00215 156.51811 143.20814 177.90269 174.09572 138.69932 103.74528
#> [120] 140.30094 191.19901 175.65901 158.23670 155.49507 147.61084 157.94692
#> [127] 178.33698 203.50118 282.89694 440.28840 315.57872 212.56633 165.57442
#> [134] 216.74543 265.35935 290.26633 268.15941 224.58420 189.01989 157.36231
#> [141] 133.63265 125.42039  99.40612 112.02728 101.00135 106.58626 102.27426
#> [148]  95.65286 102.13425 120.97199 135.99564 128.13698 123.49023 141.92174
#> [155] 138.50884 128.80946 123.28342 122.98957 119.99637 120.59551  87.69669
#> [162]  94.39083 118.23196 123.26313 132.59700 122.42059 118.91290 113.15352
#> [169] 119.33348 124.67002 122.93688  86.43318 120.69153 129.29290 111.77318
#> [176] 112.54435 118.51195 127.95338 146.05418 151.20904 140.16432 126.18718
#> [183] 126.83247 130.43882 134.20975 133.43615 140.12173 145.27918 152.67612
#> [190] 145.47517 129.38132  87.85922  83.04029  96.54220 133.72482 105.93595
#> [197] 100.88120 118.19061 102.34260 127.79007 154.63390 120.70921 139.31768
#> [204] 140.82716 160.35828 196.17869 144.13621 161.18467 184.95376 259.83103
```

Calculate the channel gradient, using the lower bank as the reference
elevation.

``` r
grad <- xt_gradient(widened_squamish, elevation = elevation_bank(min))
grad
#>   [1]            NA  3.343378e-03  5.047202e-03  2.018437e-03  7.469054e-03
#>   [6] -6.375794e-03 -2.333659e-02 -2.365805e-03  1.820774e-02  9.844091e-03
#>  [11] -3.717067e-03  2.866300e-03  7.294749e-03 -4.190876e-03 -5.724011e-03
#>  [16] -1.873562e-03  3.584672e-04 -2.586446e-03 -7.039992e-03 -5.694825e-03
#>  [21] -9.003863e-04  8.535865e-03  2.607804e-03 -1.447112e-02  1.837511e-03
#>  [26]  1.042104e-02 -1.480178e-03 -1.744544e-03 -6.525653e-04 -3.531019e-03
#>  [31] -5.187213e-03 -2.522835e-03 -2.265883e-02  1.545207e-03  5.165937e-03
#>  [36] -7.795365e-03  1.128120e-02 -1.479315e-02 -1.493079e-02  3.700181e-03
#>  [41]  4.615395e-03  3.189877e-03 -6.514638e-03 -5.946594e-04  1.833829e-02
#>  [46]  3.283490e-03 -1.831879e-02  2.557567e-04  9.131934e-03 -1.277741e-02
#>  [51] -1.062323e-02 -5.167126e-04  1.178671e-03  6.694125e-03  8.752810e-03
#>  [56] -2.750981e-04  1.417556e-03  1.196970e-02  3.166648e-03 -3.738645e-03
#>  [61]  3.229279e-03  2.107011e-03 -7.854681e-03 -1.366314e-03  4.951226e-03
#>  [66] -1.326800e-02 -1.424626e-02 -4.615457e-03 -2.440003e-02  1.495485e-02
#>  [71]  1.299592e-02 -4.373891e-03  2.362343e-02 -1.422331e-02 -1.949018e-02
#>  [76] -3.726356e-04  1.820264e-02  1.919761e-02 -2.201651e-02 -1.248190e-02
#>  [81]  6.139079e-03  1.299810e-02  6.898152e-03 -1.540897e-03 -7.650697e-03
#>  [86] -2.615487e-02  1.647857e-02  2.327266e-02  1.429889e-03  5.989410e-03
#>  [91] -8.847133e-04 -4.826104e-03 -5.942713e-03 -1.339013e-03  2.242584e-04
#>  [96] -1.477283e-03  5.779884e-04 -2.449297e-03 -1.348006e-03  8.417438e-04
#> [101]  1.559292e-04  2.493554e-03  2.673321e-03 -6.716837e-05 -1.057124e-04
#> [106] -7.274402e-03 -4.912956e-03 -1.673698e-02 -2.250308e-03  1.716891e-02
#> [111] -2.378457e-03 -1.371636e-03  6.452010e-04  4.166896e-04 -2.258977e-04
#> [116] -8.764974e-04 -6.385379e-03  3.154105e-03  5.897839e-03 -3.967785e-04
#> [121] -3.568745e-03 -8.401210e-03 -3.206543e-03  6.476167e-03 -3.904799e-03
#> [126] -2.213329e-02  7.345396e-03  2.523882e-03  2.026268e-04  6.972421e-03
#> [131]  2.074221e-03  9.517916e-03 -6.604032e-03 -1.088726e-02  4.830668e-03
#> [136]  6.207030e-03 -5.821969e-03  1.012298e-02 -1.275903e-02 -1.652513e-02
#> [141]  1.605392e-02  6.342082e-03 -3.254693e-03  2.193635e-03  8.594727e-03
#> [146]  4.496376e-03 -3.563047e-04 -6.329520e-03 -2.069294e-03 -2.193178e-03
#> [151] -4.298502e-03 -1.598858e-03 -7.260037e-03 -6.022346e-03  1.545553e-03
#> [156]  5.439126e-03  1.891023e-03 -3.228843e-03 -4.492867e-03 -6.764459e-03
#> [161]  3.356399e-03  5.631616e-03 -7.070078e-03 -5.992772e-03 -3.230371e-04
#> [166]  2.846555e-03  5.552034e-03  4.276794e-04 -7.367823e-03  1.661002e-03
#> [171]  4.106845e-03 -1.213696e-04  1.015676e-03 -6.099141e-03 -4.190938e-03
#> [176] -3.209180e-04 -6.977126e-03 -3.252441e-04  2.848133e-03 -1.912048e-03
#> [181] -7.783570e-04  1.920482e-03  1.067502e-03  2.229131e-03 -2.450008e-04
#> [186] -7.672863e-04 -2.596980e-04 -2.094002e-03  4.953337e-03  2.248919e-02
#> [191]  2.157805e-02  2.954004e-03  8.815431e-02  9.382717e-02 -1.269396e-01
#> [196] -1.184710e-01  1.289278e-02  3.338383e-03 -1.499164e-03 -3.798990e-03
#> [201]  9.113475e-03  5.536286e-03  1.426338e-03 -7.247051e-03 -1.001970e-02
#> [206] -5.829163e-03 -8.768155e-03  2.476284e-02  2.483643e-02            NA
```

Plot the gradient along the channel axis.

``` r
dist <- xt_distance_downstream(widened_squamish)
plot(dist, grad)
lines(dist, grad, type = "l")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" alt="" width="100%" />

To learn more about channel computations like these, see the [*Channel
Computations*]() vignette.

## xchan in the Context of Other R Packages

The following packages are relevant for channel geometries in R.

- The [**centerline**]() package is used to generate a centerline from a
  bankline polygon.
- The [**sf**]() package is used to manipulate vector-based spatial
  objects.
- The [**terra**]() package is used to manipulate both vector and
  raster-based spatial objects.

## Attribution

The creation of this package would not have been possible without
funding from [BGC Engineering Inc.](https://bgcengineering.ca/).
