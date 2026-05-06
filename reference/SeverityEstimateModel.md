# Severity Estimate Model

A representation of a severity estimate model and its metadata.

## Usage

``` r
SeverityEstimateModel(line_list, population)
```

## Arguments

- line_list:

  A line list of cases to model the severity of.

- population:

  A dataset containing information on the population broken down by
  strataification. Can also be a single integer in the case that the
  model is not stratafied.

## Value

A SeverityEstimateModel S4 object instance representing a model and its
associated metadata.

## Slots

- `line_list`:

  A line list of cases to model the severity of.

- `population`:

  A dataset containing information on the population broken down by
  strataification.

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
