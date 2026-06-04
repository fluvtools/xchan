# Print method for xs_profile objects

Print method for xs_profile objects

## Usage

``` r
# S3 method for class 'xs_profile'
print(x, ...)
```

## Arguments

- x:

  An xs_profile object

- ...:

  Additional arguments (ignored)

## Examples

``` r
channel <- xt_as_channel(rep(1, 3))
channel <- xt_add_profile(
  channel,
  distance = distance,
  elevation = elevation,
  section = id,
  banks = is_bank,
  data = profile_survey
)
print(channel[[1]]$profile)
#> xchan profile cross section
#>   Coordinates: 23 points
#>   Banks: 2 bank points
#>   Thalwegs: 1 thalweg points
#>   Distance range: -15 15 
#>   Elevation range: -1 4.096843 
```
