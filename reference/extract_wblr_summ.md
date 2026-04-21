# Extract WeibullR summary as a data frame

Extract WeibullR summary as a data frame

## Usage

``` r
extract_wblr_summ(wblr_obj, digits = 4)
```

## Arguments

- wblr_obj:

  Object returned by
  [`WeibullR::wblr.fit()`](https://rdrr.io/pkg/WeibullR/man/wblr.fit.html).

- digits:

  Number of decimal places to round numeric values.

## Value

A two-column data frame with `Param` and `Value`.
