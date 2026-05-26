# Check if an object has profile cross-section geometry

For an `xsection`, returns whether `profile` is non-`NULL`. For `xchan`,
returns `TRUE` only when **every** cross section carries an
`xs_profile`, and `FALSE` when **none** do.

## Usage

``` r
xt_has_profile(x)
```

## Arguments

- x:

  An `xsection` or `xchan` object.

## Value

`TRUE` if profile geometry is present as described above; `FALSE` if
uniformly absent.

## Details

Mixing sections with and without profiles is not allowed; constructors
and replacement helpers enforce this invariant when building or updating
channel geometry.

All cross sections in an `xchan` must share the same profile state (all
have `xs_profile` geometry or all omit it). If that invariant is
violated, functions that read profile state — including
`xt_has_profile()` for an `xchan` — raise an error naming which
positions differ.
