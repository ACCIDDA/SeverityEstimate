# Fit a severity estimate model instance.

Fit a severity estimate model instance.

## Usage

``` r
fit(model, ...)
```

## Arguments

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  to fit.

- ...:

  Further optional args that are eventually given to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  related to fitting.

## Value

A
[SeverityEstimateFit](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
S4 object.

## Examples

``` r
if (FALSE) { # \dontrun{
fit(default_model(line_list, population), chains = 1L, iter = 100L)
} # }
```
