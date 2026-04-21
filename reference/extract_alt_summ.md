# Extract ALT summary as a data frame

Extract ALT summary as a data frame

## Usage

``` r
extract_alt_summ(alt_obj, digits = 4)
```

## Arguments

- alt_obj:

  Object returned by
  [`WeibullR.ALT::alt.fit()`](https://rdrr.io/pkg/WeibullR.ALT/man/alt.fit.html).

- digits:

  Number of decimal places to round numeric values.

## Value

A two-column data frame with `Param` and `Value`.
