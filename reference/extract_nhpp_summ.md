# Extract NHPP summary as a data frame

Extract NHPP summary as a data frame

## Usage

``` r
extract_nhpp_summ(nhpp_obj, digits = 4)
```

## Arguments

- nhpp_obj:

  Object returned by
  [`ReliaGrowR::nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.html).

- digits:

  Number of decimal places to round numeric values.

## Value

A two-column data frame with `Param` and `Value`.
