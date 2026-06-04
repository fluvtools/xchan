# Side Schemes

Side functions determine how much of the widening applied to a channel
is allocated to the left and right banks. They return side objects as
lists with `left` and `right` proportions.

## Usage

``` r
side_left(prop = 1)

side_right(prop = 1)

side_both(prop_left = 0.5, prop_right = 0.5)
```

## Arguments

- prop:

  A single numeric value between 0 and 1 indicating how much of the
  widening to apply to the specified bank.

- prop_left, prop_right:

  Single numeric values between 0 and 1 indicating how much of the
  widening to apply to the left and right banks, respectively.

## Value

A side object that can be used in widening functions. It is a list with
numeric entries `left` and `right`.

## Details

While these functions are different ways of specifying the same thing,
they are included for completeness. An advantage of using the
`side_both()` function is that it conducts an internal check that the
proportions for the left and right banks sum to 1.

## Examples

``` r
side_both(prop_left = 0.5, prop_right = 0.5)
#> Side allocation specification.
#> Proportions:
#>   left = 0.5 
#>   right = 0.5 
side_left(0.7)
#> Side allocation specification.
#> Proportions:
#>   left = 0.7 
#>   right = 0.3 
side_right(0.3)
#> Side allocation specification.
#> Proportions:
#>   left = 0.7 
#>   right = 0.3 
channel <- xt_as_channel(rep(1, 3))
xt_widen(channel, dw = 5, side = side_left(0.75))
#> xchan channel with 3 cross sections.
#> <xsection 1> 6 (-)
#> <xsection 2> 6 (-)
#> <xsection 3> 6 (-)
```
