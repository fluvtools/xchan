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
