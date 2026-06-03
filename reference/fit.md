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
# \donttest{
set.seed(1)
line_list <- data.frame(
  patient = 1L:3L,
  week = c(1L, 1L, 2L),
  age = c("Youth", "Adult", "Senior"),
  detection = c("Active", "Passive", "Active"),
  outcome = c("Asymptomatic", "Death", "Symptomatic")
)
population <- data.frame(
  age = c("Youth", "Adult", "Senior"),
  amount = rep(987L, 3L)
)
fitted_model <- fit(
  default_model(line_list, population),
  chains = 1L,
  cores = 1L,
  iter = 10L,
  warmup = 5L,
  seed = 1,
  refresh = 0
)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'timesteps': `line_list` is missing required columns: time.
# }
```
