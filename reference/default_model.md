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
#> Severity Estimate Model:
#> 
#> Data:
#>     dataset rows columns
#>   line_list    3       4
#>  population    3       2
#> 
#> Detection Probability Priors:
#>   active prior: beta(1.0, 1.0)
#>   passive_asymptomatic prior: beta(1.0, 3.0)
#>   passive_symptomatic prior: beta(3.0, 1.0)
#> 
#> Timesteps:
#>   time: 1 to 2 (2 timesteps)
#> 
#> Detection:
#>   column: detection
#>     active: 2 cases (values: Active)
#>     passive: 1 cases (values: Passive)
#> 
#> Outcome:
#>   column: outcome
#>     asymptomatic: 1 cases (values: Asymptomatic)
#>     symptomatic: 1 cases (values: Symptomatic)
#>     severe: 1 cases (values: Death)
#> 
#> Strata:
#>   age: 3 levels, df = 0 (Adult, Senior, Youth)
```
