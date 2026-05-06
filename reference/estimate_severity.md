# Estimate Severity From A Line List

Fit a severity estimate model accounting for under reporting of
asymptomatic and mildly symptomatic cases.

## Usage

``` r
estimate_severity(
  linelist,
  population,
  surveillance,
  outcome,
  time_period = character(),
  strata = character(),
  population_value = utils::tail(names(population), n = 1L),
  surveillance_reference = NULL,
  outcome_reference = NULL,
  time_period_reference = NULL,
  strata_reference = NULL,
  hazard_std = 3,
  degrees_of_freedom = 1L,
  active_prior = c(alpha = 1, beta = 25),
  passive_asymptomatic_prior = c(alpha = 1, beta = 2),
  passive_symptomatic_prior = c(alpha = 25, beta = 1),
  ...
)
```

## Arguments

- linelist:

  A `data.frame` (or `data.frame` extending object like a `tibble`) of
  the line list data.

- population:

  A `data.frame` (or `data.frame` extending object like a `tibble`) of
  the population data.

- surveillance:

  A character of columns describing the surveillance methods. The values
  in this column must be coercible to 'Active' and 'Passive'.

- outcome:

  A character of columns describing the outcome of the line list entry.
  The values in this column must be coercible to 'Asymptomatic',
  'Death', and 'Symptomatic'.

- time_period:

  A character of columns describing the time period such as 'week' or
  'day'

- strata:

  A character of columns describing the attributes to stratify the data
  on.

- population_value:

  A unit length character corresponding to the column in the
  `population` data.frame (or data.frame like object) for the population
  value.

- surveillance_reference:

  Either `NULL`, a `data.frame`, or a vector of values to use as a
  reference for the surveillance dimension. If a vector then it is
  expected that `surveillance` is unit length.

- outcome_reference:

  Either `NULL`, a `data.frame`, or a vector of values to use as a
  reference for the outcome dimension. If a vector then it is expected
  that `outcome` is unit length.

- time_period_reference:

  Either `NULL`, a `data.frame`, or a vector of values to use as a
  reference for the time period dimension. If a vector then it is
  expected that `time_period` is unit length.

- strata_reference:

  Either `NULL`, a `data.frame`, or a vector of values to use as a
  reference for the strata dimension. If a vector then it is expected
  that `strata` is unit length.

- hazard_std:

  A single length numeric greater than zero for the standard deviation
  to use in the community hazard prior distribution.

- degrees_of_freedom:

  A single length integer greater than zero for the degrees of freedom
  to use in the model.

- active_prior:

  The parameters for the prior beta distribution for the active
  detection probability. Can be specified as 'alpha'/'beta',
  'mean'/'var', or 'mean'/'sd'.

- passive_asymptomatic_prior:

  The parameters for the prior beta distribution for the passive
  asymptomatic detection probability. Can be specified as
  'alpha'/'beta', 'mean'/'var', or 'mean'/'sd'.

- passive_symptomatic_prior:

  The parameters for the prior beta distribution for the passive
  symptomatic detection probability. Can be specified as 'alpha'/'beta',
  'mean'/'var', or 'mean'/'sd'.

- ...:

  Further optional args that are eventually given to
  [`rstan::stan()`](https://mc-stan.org/rstan/reference/stan.html)
  related to fitting.

## Value

A
[SeverityEstimateFit](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
S4 object.
