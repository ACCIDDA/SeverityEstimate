# Logit And Inverse Logit

Math utilities for converting between a probability and real valued
numbers.

## Usage

``` r
logit(p)

inv_logit(x)
```

## Arguments

- p:

  A probability to convert to a real value.

- x:

  A real value to convert to probability.

## Value

`logit()` returns the log odds for `p`; `inv_logit()` returns the
probability corresponding to `x`.

## Details

These functions are named to mimic their stan equivalents.
