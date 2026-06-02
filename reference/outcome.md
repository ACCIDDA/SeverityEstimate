# Get Or Set Model Outcome Mapping

S4 getter and replacement methods for the `outcome` slot on a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md),
plus chainable helpers to set and validate outcome readiness.

## Usage

``` r
outcome(x)

outcome(x) <- value

# S4 method for class 'SeverityEstimateModel'
outcome(x)

has_outcome(x)

# S4 method for class 'SeverityEstimateModel'
has_outcome(x)

# S4 method for class 'SeverityEstimateModel'
outcome(x) <- value

require_outcome(model, mode = "error")

set_outcome(
  model,
  name,
  map = c(asymptomatic = "asymptomatic", symptomatic = "symptomatic", severe = "severe")
)
```

## Arguments

- x:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- value:

  A named list with entries `name` and `map`.

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- mode:

  How `require_outcome()` should respond when outcome is not set. One of
  `error`, `warn`, `silent`.

- name:

  The name of the outcome column, which must be present in `line_list`.

- map:

  A named character vector mapping outcome column values to one of
  `asymptomatic`, `symptomatic`, or `severe`.

## Value

`outcome(x)` returns the current outcome specification.

`outcome(x) <- value` returns `x` modified to include the outcome
specification.

`has_outcome(x)` returns `TRUE` if outcome has been set, `FALSE`
otherwise.

`require_outcome(model, mode)` returns `model`. If outcome is unset, the
behavior depends on `mode`.

`set_outcome(model, ...)` returns `model` modified to include the
outcome specification.

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
model <- SeverityEstimateModel(line_list, population) |>
  set_outcome("outcome", map = c(
    "Asymptomatic" = "asymptomatic",
    "Symptomatic" = "symptomatic",
    "Death" = "severe"
  ))
model
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
#>   column: outcome
#>     asymptomatic: 1 cases (values: Asymptomatic)
#>     symptomatic: 1 cases (values: Symptomatic)
#>     severe: 1 cases (values: Death)
#> 
#> Strata:
#>   none
```
