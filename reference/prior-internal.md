# Internal Prior Setter Helper

Internal helper used by replacement methods to validate and set prior
parameterizations on a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

Internal helper used by getter methods to extract configured priors from
a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md),
returning a default prior with warning if unset.

## Usage

``` r
prior(model, parameter, ...)

get_prior(model, parameter)
```

## Arguments

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- parameter:

  The model parameter to get the prior for, should be one of `active`,
  `passive_asymptomatic`, `passive_symptomatic`.

- ...:

  Beta distribution parameterization. Must be one of `alpha`/`beta`,
  `mean`/`var`, `mean`/`sd`, `mean`/`concentration`.

## Value

`model` modified to contain the prior parameterization for `parameter`.

A named numeric vector of `alpha` and `beta`.
