# Get Or Set Model Detection Mapping

S4 getter and replacement methods for the `detection` slot on a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md),
plus chainable helpers to set and validate detection readiness.

## Usage

``` r
detection(x)

detection(x) <- value

# S4 method for class 'SeverityEstimateModel'
detection(x)

has_detection(x)

# S4 method for class 'SeverityEstimateModel'
has_detection(x)

# S4 method for class 'SeverityEstimateModel'
detection(x) <- value

require_detection(model, mode = "error")

set_detection(model, name, map = c(active = "active", passive = "passive"))
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

  How `require_detection()` should respond when detection is not set.
  One of `error`, `warn`, `silent`.

- name:

  The name of the detection column, which must be present in
  `line_list`.

- map:

  A named character vector mapping detection column values to either
  `active` or `passive`.

## Value

`detection(x)` returns the current detection specification.

`detection(x) <- value` returns `x` modified to include the detection
specification.

`has_detection(x)` returns `TRUE` if detection has been set, `FALSE`
otherwise.

`require_detection(model, mode)` returns `model`. If detection is unset,
the behavior depends on `mode`.

`set_detection(model, ...)` returns `model` modified to include the
detection specification.

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
  set_detection(
    "detection",
    map = c("Active" = "active", "Passive" = "passive")
  )
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
#>   ..@ detection                 :List of 2
#>   .. ..$ name: chr "detection"
#>   .. ..$ map : Named chr [1:2] "active" "passive"
#>   .. .. ..- attr(*, "names")= chr [1:2] "Active" "Passive"
#>   ..@ outcome                   : list()
#>   ..@ active_prior              : num(0) 
#>   ..@ passive_asymptomatic_prior: num(0) 
#>   ..@ passive_symptomatic_prior : num(0) 
```
