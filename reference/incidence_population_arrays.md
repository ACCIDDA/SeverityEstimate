# Convert Incidence And Populations To Like Shaped Arrays

Create an incidence and population arrays that are compatible in shape
from a data.frame like line list and population.

## Usage

``` r
incidence_population_arrays(
  linelist,
  population,
  time_period,
  strata,
  surveillance,
  outcome,
  population_value,
  time_period_reference,
  strata_reference,
  surveillance_reference,
  outcome_reference
)
```

## Arguments

- linelist:

  A line list to convert into an array.

- population:

  Population data set to convert into an array.

- time_period:

  A character of columns describing the time period such as 'week' or
  'day'

- strata:

  A character of columns describing the attributes to stratify the data
  on.

- surveillance:

  A character of columns describing the surveillance methods.

- outcome:

  A character of columns describing the outcome of the line list entry.

- population_value:

  A unit length character corresponding to the column in the
  `population` data.frame (or data.frame like object) for the population
  value.

## Value

A list with names 'incidence', 'population', 'time_period', 'strata',
'surveillance', and 'outcome'. These named list elements correspond to:

- `incidence`: A four dimensional array with the dimensions referring to
  `time_period`, `strata`, `surveillance`, and `outcome`.

- `population`: A one dimensional array with the dimensions referring to
  `strata`.

- `time_period`: A data.frame constructed from the `time_period` columns
  given and whose rownames correspond to the `time_period` dimension.

- `strata`: A data.frame constructed from the `strata` columns given and
  whose rownames correspond to the `strata` dimension.

- `surveillance`: A data.frame constructed from the `surveillance`
  columns given and whose rownames correspond to the `surveillance`
  dimension.

- `outcome`: A data.frame constructed from the `outcome` columns given
  and whose rownames correspond to the `outcome` dimension.

- `linelist_ind`: An integer matrix with the same number of rows as
  `linelist` and columns corresponding to the dimensions extracted. The
  values indicate where each line list element corresponds to in each of
  those dimensions.

The strata dimensions of `incidence` and `population` will be the same.

## Details

It is expected that all of the `time_period`, `strata`, `surveillance`,
and `outcome` columns are all present in `linelist`. All of the `strata`
columns must be in `population`.
