# Resolve Beta Distribution Parameterization

Resolve Beta Distribution Parameterization

## Usage

``` r
beta_parameterization(params)
```

## Arguments

- params:

  A named numeric of length two that represent the parameterization of a
  beta distribution. Names must be one of 'alpha'/'beta', 'mean'/'var',
  'mean'/'sd', or 'mean'/'concentration'.

## Value

A numeric of length two with names 'alpha' and 'beta' corresponding to
the `params` converted to alpha/beta parameterization.
