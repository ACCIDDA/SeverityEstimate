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
#>   ..@ strata                    :List of 1
#>   .. ..$ :List of 3
#>   .. .. ..$ name              : chr "age"
#>   .. .. ..$ levels            : chr [1:3] "Youth" "Adult" "Senior"
#>   .. .. ..$ degrees_of_freedom: int 0
#>   ..@ timesteps                 : list()
#>   ..@ detection                 : list()
#>   ..@ outcome                   : list()
#>   ..@ active_prior              : num(0) 
#>   ..@ passive_asymptomatic_prior: num(0) 
#>   ..@ passive_symptomatic_prior : num(0) 
```
