# Print method for xchan_side objects

Prints the left/right proportions only (constructors
[`side_left()`](https://fluvtools.github.io/xchan/reference/sides.md),
etc., are equivalent).

## Usage

``` r
# S3 method for class 'xchan_side'
print(x, ...)
```

## Arguments

- x:

  A side object

- ...:

  Additional arguments (ignored)

## Examples

``` r
print(side_both())
#> Side allocation specification.
#> Proportions:
#>   left = 0.5 
#>   right = 0.5 
```
