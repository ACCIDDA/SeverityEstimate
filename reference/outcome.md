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
#> Formal class 'SeverityEstimateModel' [package "SeverityEstimate"] with 9 slots
#>   ..@ line_list                 :'data.frame':   3 obs. of  5 variables:
#>   .. ..$ patient  : int [1:3] 1 2 3
#>   .. ..$ week     : int [1:3] 1 1 2
#>   .. ..$ age      : chr [1:3] "Youth" "Adult" "Senior"
#>   .. ..$ detection: chr [1:3] "Active" "Passive" "Active"
#>   .. ..$ outcome  : chr [1:3] "Asymptomatic" "Death" "Symptomatic"
#>   ..@ population                :'data.frame':   3 obs. of  2 variables:
#>   .. ..$ age   : chr [1:3] "Youth" "Adult" "Senior"
#>   .. ..$ amount: int [1:3] 987 987 987
#>   ..@ strata                    : list()
#>   ..@ timesteps                 : list()
#>   ..@ detection                 : list()
#>   ..@ outcome                   :List of 2
#>   .. ..$ name: chr "outcome"
#>   .. ..$ map : Named chr [1:3] "asymptomatic" "symptomatic" "severe"
#>   .. .. ..- attr(*, "names")= chr [1:3] "Asymptomatic" "Symptomatic" "Death"
#>   ..@ active_prior              : num(0) 
#>   ..@ passive_asymptomatic_prior: num(0) 
#>   ..@ passive_symptomatic_prior : num(0) 
```
