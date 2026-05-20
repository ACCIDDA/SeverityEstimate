# Construct A Default Severity Model

Construct a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
from already-formatted line list and population data by inferring the
model structure from column names.

The `line_list` must contain `time`, `detection`, and `outcome` columns.
Every other `line_list` column is treated as a strata column with
`degrees_of_freedom = 0L`, so those columns must also be present in
`population`. The `population` data must then contain exactly one
additional non-strata column, which is treated as the population count
column.

Detection values must be case-insensitive forms of `active`/`passive` or
`a`/`p`. Outcome values must be case-insensitive forms of
`asymptomatic`/`symptomatic`/`death` or `a`/`s`/`d`.

The returned model includes weakly informative detection priors suitable
for fitting immediately with
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md).

## Usage

``` r
default_model(line_list, population)
```

## Arguments

- line_list:

  A `data.frame` (or `data.frame` extending object like a `tibble`) of
  line list data.

- population:

  A `data.frame` (or `data.frame` extending object like a `tibble`) of
  population data.

## Value

A
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
S4 object instance.

## Examples

``` r
line_list <- data.frame(
  time = c(1L, 1L, 2L),
  age = c("Youth", "Adult", "Senior"),
  detection = c("Active", "Passive", "Active"),
  outcome = c("Asymptomatic", "Death", "Symptomatic")
)
population <- data.frame(
  age = c("Youth", "Adult", "Senior"),
  value = c(1000L, 1200L, 900L)
)
model <- default_model(line_list, population)
model
#> Formal class 'SeverityEstimateModel' [package "SeverityEstimate"] with 9 slots
#>   ..@ line_list                 :'data.frame':   3 obs. of  4 variables:
#>   .. ..$ time     : int [1:3] 1 1 2
#>   .. ..$ age      : chr [1:3] "Youth" "Adult" "Senior"
#>   .. ..$ detection: chr [1:3] "Active" "Passive" "Active"
#>   .. ..$ outcome  : chr [1:3] "Asymptomatic" "Death" "Symptomatic"
#>   ..@ population                :'data.frame':   3 obs. of  2 variables:
#>   .. ..$ age  : chr [1:3] "Youth" "Adult" "Senior"
#>   .. ..$ value: int [1:3] 1000 1200 900
#>   ..@ strata                    :List of 1
#>   .. ..$ :List of 3
#>   .. .. ..$ name              : chr "age"
#>   .. .. ..$ levels            : chr [1:3] "Adult" "Senior" "Youth"
#>   .. .. ..$ degrees_of_freedom: int 0
#>   ..@ timesteps                 :List of 2
#>   .. ..$ name  : chr "time"
#>   .. ..$ levels: int [1:2] 1 2
#>   ..@ detection                 :List of 2
#>   .. ..$ name: chr "detection"
#>   .. ..$ map : Named chr [1:2] "active" "passive"
#>   .. .. ..- attr(*, "names")= chr [1:2] "Active" "Passive"
#>   ..@ outcome                   :List of 2
#>   .. ..$ name: chr "outcome"
#>   .. ..$ map : Named chr [1:3] "asymptomatic" "severe" "symptomatic"
#>   .. .. ..- attr(*, "names")= chr [1:3] "Asymptomatic" "Death" "Symptomatic"
#>   ..@ active_prior              : Named num [1:2] 1 1
#>   .. ..- attr(*, "names")= chr [1:2] "alpha" "beta"
#>   ..@ passive_asymptomatic_prior: Named num [1:2] 1 3
#>   .. ..- attr(*, "names")= chr [1:2] "alpha" "beta"
#>   ..@ passive_symptomatic_prior : Named num [1:2] 3 1
#>   .. ..- attr(*, "names")= chr [1:2] "alpha" "beta"
```
