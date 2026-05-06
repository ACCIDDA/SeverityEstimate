# Severity Estimate Fit Class

This class contains the output from a severity estimate model fitting.

## Slots

- `model_fit`:

  A stanfit object returned from fitting a severity estimate model.

- `population`:

  The population data used in model fitting in array form with
  dimensions corresponding to 'time_period' and 'strata'.

- `incidence`:

  The line list data used in model fitting in array form counting
  incidence with dimensions corresponding to 'time_period', 'strata',
  'surveillance', and 'outcome'.

- `time_period`:

  A data.frame with the variables describing the 'time_period'
  dimensions of `population` and `incidence`.

- `strata`:

  A data.frame with the variables describing the 'strata' dimensions of
  `population` and `incidence`.

- `surveillance`:

  A data.frame with the variables describing the 'surveillance'
  dimension of `incidence`.

- `outcome`:

  A data.frame with the variables describing the 'outcome' dimension of
  `incidence`.
