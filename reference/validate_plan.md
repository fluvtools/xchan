# Validate plan view cross sections

Check if plan view cross sections (line segments) are proper for cross
section analysis.

## Usage

``` r
validate_plan(plan)
```

## Arguments

- plan:

  Plan view cross sections as sfc_LINESTRING object

## Value

A list with validation results:

- `valid`: Logical indicating if all checks passed - `issues`: Character
  vector describing any issues found - `details`: List with detailed
  results for each check

## Details

This function performs basic validation on plan view cross sections:

- **Basic geometry**: Ensures all geometries are valid LINESTRING
  objects
