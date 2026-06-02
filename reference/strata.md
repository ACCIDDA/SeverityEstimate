# Get Or Set Model Stratifications

S4 getter and replacement methods for the `strata` slot on a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md),
plus a chainable `set_strata()` helper for pipeline ergonomics.

## Usage

``` r
strata(x)

strata(x) <- value

# S4 method for class 'SeverityEstimateModel'
strata(x)

# S4 method for class 'SeverityEstimateModel'
strata(x) <- value

set_strata(model, name, levels = NULL, degrees_of_freedom = NULL)
```

## Arguments

- x:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- value:

  A named list with entries `name`, `levels`, and `degrees_of_freedom`.

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- name:

  The name of the stratification column, which must be present in both
  the `line_list` and `population` `data.frame`s.

- levels:

  The levels for the stratification, or `NULL` to infer from
  `line_list`/`population`.

- degrees_of_freedom:

  The degrees of freedom for the strata fixed effects. `NULL` and `0L`
  use unsmoothed categorical effects. Values greater than `0L` request
  an ordered smooth effect and therefore require explicit `levels`. The
  value must be less than the saturated categorical fit, i.e. at most
  `length(levels) - 2L`.

## Value

`strata(x)` returns the current list of model stratifications.

`strata(x) <- value` returns `x` modified to include the given
stratification.

`set_strata(model, ...)` returns `model` modified to include the given
stratification.

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
  set_strata("age", levels = c("Youth", "Adult", "Senior"))
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
#>   not set
#> 
#> Strata:
#>   age: 3 levels, df = 0 (Youth, Adult, Senior)
```
