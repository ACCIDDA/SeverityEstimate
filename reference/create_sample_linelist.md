# Create A Sample Line List

Generates a sample line list that can be used as an example data set for
analysis with this package.

## Usage

``` r
create_sample_linelist(
  strata,
  times,
  active_detection,
  passive_asymptomatic_detection,
  passive_symptomatic_detection,
  force_of_infection = NULL,
  force_of_infection_mean = -5,
  seed = 1L
)
```

## Arguments

- strata:

  A data.frame like object with the columns 'population', 'sir', and
  'ifr'. Any additional columns are treated as descriptions for the
  strata. If `strata` only contains the columns 'population', 'sir',
  'ifr' it's assumed that the sample data set contains no strata.

- times:

  A vector denoting time steps for the sample data set. Must be provided
  in order.

- active_detection:

  The probability of detecting a case through active detection.

- passive_asymptomatic_detection:

  The probability of detecting a case presenting asymptomatically
  through passive surveillance.

- passive_symptomatic_detection:

  The probability of detecting a case presenting symptomatically through
  passive surveillance.

- force_of_infection:

  A matrix representing the force of infection where the row dimension
  corresponds to the `times` given and the column dimension corresponds
  to the `strata` given or `NULL` to generate a random one.

- force_of_infection_mean:

  The mean of the initial force of infection to use. Only used when
  `force_of_infection` is `NULL`.

- seed:

  The random seed to use for generating the data set.

## Value

A data.frame with the columns 'patient', 'time', 'detection', 'outcome'
along with the strata columns provided in `strata`.
