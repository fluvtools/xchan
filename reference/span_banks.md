# Generate a line segment from bank to bank

Given a point within a channel, generate a line segment that goes from
bank to bank, for a specified angle.

## Usage

``` r
span_banks(pt, angle, bankline)
```

## Arguments

- pt:

  A point within the channel.

- angle:

  The angle of the line segment, in radians.

- bankline:

  The bankline of the channel.

## Value

A line segment spanning from bank to bank.

## Note

This function is the precursor to generating cross sections with
`xt_generate_xsc()`.

## Examples

``` r
# Pick a point in the channel, and an angle
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
pt <- st_centroid(Squamish_bankline) - c(0, 30)
plot(Squamish_bankline)
plot(pt, add = TRUE)


## 45 degrees:
span <- span_banks(pt, angle = pi / 4, bankline = Squamish_bankline)
#> Error in span_banks(pt, angle = pi/4, bankline = Squamish_bankline): could not find function "span_banks"
plot(span, add = TRUE, col = "blue")
#> Error: object 'span' not found

## 0 degrees:
span <- span_banks(pt, angle = 0, bankline = Squamish_bankline)
#> Error in span_banks(pt, angle = 0, bankline = Squamish_bankline): could not find function "span_banks"
plot(span, add = TRUE, col = "blue")
#> Error: object 'span' not found
```
