# Inject points into a profile cross section matrix

Inject a point into a 2D cross section matrix, potentially splitting a
linesegment into two if x doesn't already land on a node.

## Usage

``` r
inject_coords(profile, x)
```

## Arguments

- profile:

  Cross section profile.

- x:

  Numeric vector; distance along cross section to add a new node to.

## Value

The original profile cross section, with additional nodes corresponding
to `x`, with linearly interpolated elevation.
