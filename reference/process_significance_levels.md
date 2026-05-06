# Convert Significance Levels Into Confidence Interval Bounds

Calculate the lower and upper bounds of a confidence interval from a
significance level.

## Usage

``` r
process_significance_levels(alpha)
```

## Arguments

- alpha:

  A numeric of significance levels to consider. Only the first two
  decimals are considered and the values in this numeric must be unique.

## Value

A numeric matrix of two rows, with dimnames "lower" and "upper" and
`length(alpha)` columns, with dimnames of `alpha` formatted as a two
digit number like '05' or '10' for `0.05` or `0.1`.
