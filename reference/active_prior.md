# Get Or Set Model Prior Parameterizations

S4 getter and replacement methods for prior parameterizations on a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md),
plus chainable `set_*` helpers for pipeline ergonomics.

## Usage

``` r
active_prior(x)

active_prior(x) <- value

# S4 method for class 'SeverityEstimateModel'
active_prior(x)

# S4 method for class 'SeverityEstimateModel'
active_prior(x) <- value

set_active_prior(model, ...)

passive_asymptomatic_prior(x)

passive_asymptomatic_prior(x) <- value

# S4 method for class 'SeverityEstimateModel'
passive_asymptomatic_prior(x)

# S4 method for class 'SeverityEstimateModel'
passive_asymptomatic_prior(x) <- value

set_passive_asymptomatic_prior(model, ...)

passive_symptomatic_prior(x)

passive_symptomatic_prior(x) <- value

# S4 method for class 'SeverityEstimateModel'
passive_symptomatic_prior(x)

# S4 method for class 'SeverityEstimateModel'
passive_symptomatic_prior(x) <- value

set_passive_symptomatic_prior(model, ...)
```

## Arguments

- x:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- value:

  A named list or named numeric vector providing a beta
  parameterization. Must be one of `alpha`/`beta`, `mean`/`var`,
  `mean`/`sd`, `mean`/`concentration`.

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- ...:

  Beta distribution parameterization. Must be one of `alpha`/`beta`,
  `mean`/`var`, `mean`/`sd`, `mean`/`concentration`.

## Value

`active_prior(x)` returns the current active prior parameterization. If
the active prior has not yet been set, a warning is issued and the
default prior `c(alpha = 1.0, beta = 1.0)` is returned.

`active_prior(x) <- value` returns `x` modified to contain the active
prior parameterization.

`set_active_prior(model, ...)` returns `model` modified to contain the
active prior parameterization.

`passive_asymptomatic_prior(x)` returns the current passive asymptomatic
prior parameterization. If the passive asymptomatic prior has not yet
been set, a warning is issued and the default prior
`c(alpha = 1.0, beta = 1.0)` is returned.

`passive_asymptomatic_prior(x) <- value` returns `x` modified to contain
the passive asymptomatic prior parameterization.

`set_passive_asymptomatic_prior(model, ...)` returns `model` modified to
contain the passive asymptomatic prior parameterization.

`passive_symptomatic_prior(x)` returns the current passive symptomatic
prior parameterization. If the passive symptomatic prior has not yet
been set, a warning is issued and the default prior
`c(alpha = 1.0, beta = 1.0)` is returned.

`passive_symptomatic_prior(x) <- value` returns `x` modified to contain
the passive symptomatic prior parameterization.

`set_passive_symptomatic_prior(model, ...)` returns `model` modified to
contain the passive symptomatic prior parameterization.

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
  set_active_prior(mean = 0.9, concentration = 12.5) |>
  set_passive_asymptomatic_prior(alpha = 1.0, beta = 1.0) |>
  set_passive_symptomatic_prior(mean = 0.1, var = 0.0064)
model
#> Severity Estimate Model:
#> 
#> Data:
#>     dataset rows columns
#>   line_list    3       5
#>  population    3       2
#> 
#> Detection Probability Priors:
#>   active prior: beta(11.25, 1.25)
#>   passive_asymptomatic prior: beta(1.0, 1.0)
#>   passive_symptomatic prior: beta(1.306, 11.76)
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
