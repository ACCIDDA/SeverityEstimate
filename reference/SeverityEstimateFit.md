# Severity Estimate Fit Class

This class contains the output from a severity estimate model fitting.

## Usage

``` r
# S3 method for class 'SeverityEstimateFit'
print(x, ...)

# S3 method for class 'SeverityEstimateFit'
summary(object, ...)

# S3 method for class 'SummaryEstimateFit'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- x:

  An object of class SeverityEstimateFit or `SummaryEstimateFit`.

- ...:

  For `summary.SeverityEstimateFit()`, unused. For
  `print.SeverityEstimateFit()`, further arguments passed to the `print`
  method for a `stanfit` object. For `print.SummaryEstimateFit()`,
  further arguments passed to
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html).

- object:

  An object of class SeverityEstimateFit.

- digits:

  The number of significant digits to print.

## Value

A function-dependent value:

- `summary.SeverityEstimateFit()` returns a `SummaryEstimateFit` with
  elements `detection_rates` and `severity_estimates`.

- `print.SeverityEstimateFit()` and `print.SummaryEstimateFit()`
  invisibly return their input object.

## Slots

- `model_fit`:

  A stanfit object returned from fitting a severity estimate model.

- `population`:

  The population data used in model fitting in array form with
  dimensions corresponding to 'time_period' and 'strata'.

- `incidence`:

  The line list data used in model fitting in array form counting
  incidence with dimensions corresponding to 'time_period', 'strata',
  'surveillance', and 'outcome'.

- `time_period`:

  A data.frame with the variables describing the 'time_period'
  dimensions of `population` and `incidence`.

- `strata`:

  A data.frame with the variables describing the 'strata' dimensions of
  `population` and `incidence`.

- `surveillance`:

  A data.frame with the variables describing the 'surveillance'
  dimension of `incidence`.

- `outcome`:

  A data.frame with the variables describing the 'outcome' dimension of
  `incidence`.

## Functions and methods

- `summary(object)` summarises a fitted severity estimate model by
  reporting mean detection rate estimates and mean IFR/SIR estimates by
  strata.

- `print.SeverityEstimateFit(x)` prints a SeverityEstimateFit object in
  a structured format. Currently this prints the `model_fit` slot using
  the `print` method for a `stanfit` object.

- `print.SummaryEstimateFit(x, digits)` prints a `SummaryEstimateFit`
  object in a structured format.

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
model <- default_model(line_list, population)
#> Error: `line_list` is missing required columns: time.
fitted_model <- fit(
  model,
  chains = 1L,
  cores = 1L,
  iter = 10L,
  warmup = 5L,
  seed = 1,
  refresh = 0
)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'timesteps': object 'model' not found
summary(fitted_model)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'object' in selecting a method for function 'summary': object 'fitted_model' not found
# }
```
