# Construct a single cross section (`xsection`)

`xsection` stores one planimetric transect (required) and an optional
profile cross section. Plan geometry is stored as a numeric matrix of
`(x, y)` pairs with rows ordered from left bank to right bank.

## Usage

``` r
xsection(plan, profile = NULL)
```

## Arguments

- plan:

  Matrix with 2 numeric columns (`x`, `y`) and at least 2 rows.

- profile:

  Optional `xs_profile` object.

## Value

An object of class `"xsection"`.
