# Extract RGA summary as a data frame

Extract RGA summary as a data frame

## Usage

``` r
extract_rga_summ(rga_obj, digits = 4)
```

## Arguments

- rga_obj:

  Object returned by
  [`ReliaGrowR::rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.html).

- digits:

  Number of decimal places to round numeric values.

## Value

A two-column data frame with `Param` and `Value`.
