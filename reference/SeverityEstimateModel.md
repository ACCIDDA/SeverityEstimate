# Severity Estimate Model

A representation of a severity estimate model and its metadata.

## Usage

``` r
SeverityEstimateModel(line_list, population)

# S3 method for class 'SeverityEstimateModel'
summary(object, ...)

# S3 method for class 'SeverityEstimateModel'
print(x, ...)

# S4 method for class 'SeverityEstimateModel'
show(object)

# S3 method for class 'SummaryEstimateModel'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- line_list:

  A line list of cases to model the severity of.

- population:

  A dataset containing information on the population broken down by
  stratification. Can also be a single integer in the case that the
  model is not stratified.

- object:

  An object of class SeverityEstimateModel.

- ...:

  For `summary.SeverityEstimateModel()` and
  `print.SummaryEstimateModel()`, unused. For
  `print.SeverityEstimateModel()`, further arguments passed to
  `print.SummaryEstimateModel()`.

- x:

  An object of class SeverityEstimateModel or `SummaryEstimateModel`.

- digits:

  The number of significant digits to print for prior parameters.

## Value

A function-dependent value:

- `SeverityEstimateModel()` returns a SeverityEstimateModel object.

- `summary.SeverityEstimateModel()` returns a `SummaryEstimateModel`.

- `print.SeverityEstimateModel()`,
  [`show()`](https://rdrr.io/r/methods/show.html), and
  `print.SummaryEstimateModel()` invisibly return their input object.

## Slots

- `line_list`:

  A line list of cases to model the severity of.

- `population`:

  A dataset containing information on the population broken down by
  stratification.

- `strata`:

  A list of model stratification specifications.

- `timesteps`:

  A list specifying the timestep column of the linelist.

- `detection`:

  A list specifying the detection type mapping.

- `outcome`:

  A list specifying the outcome severity mapping.

- `active_prior`:

  Parameters for the beta distribution prior for the active detection
  rate.

- `passive_asymptomatic_prior`:

  Parameters for the beta distribution prior for the passive
  asymptomatic detection rate.

- `passive_symptomatic_prior`:

  Parameters for the beta distribution prior for the passive symptomatic
  detection rate.

## Functions and methods

- `SeverityEstimateModel(line_list, population)` creates a
  SeverityEstimateModel object.

- `summary(object)` summarises a user-defined severity estimate model by
  reporting input data dimensions, detection probability priors,
  timestep bounds, mapped detection and outcome counts, and strata
  specifications.

- `print.SeverityEstimateModel(x)` prints a compact summary of a
  SeverityEstimateModel object.

- `show(object)` shows a compact summary of a SeverityEstimateModel
  object.

- `print.SummaryEstimateModel(x, digits)` prints a
  `SummaryEstimateModel` object in a structured format.

## Examples

``` r
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
model <- SeverityEstimateModel(line_list, population)
summary(model)
#> Severity Estimate Model:
#> 
#> Data:
#>     dataset rows columns
#>   line_list    3       5
#>  population    3       2
#> 
#> Detection Probability Priors:
#>   active prior: beta(1.0, 1.0) (default)
#>   passive_asymptomatic prior: beta(1.0, 1.0) (default)
#>   passive_symptomatic prior: beta(1.0, 1.0) (default)
#> 
#> Timesteps:
#>   not set
#> 
#> Detection:
#>   not set
#> 
#> Outcome:
#>   not set
#> 
#> Strata:
#>   none
```
