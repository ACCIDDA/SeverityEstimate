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

## Examples

``` r
strata <- data.frame(
  age = c("Adult", "Senior"),
  population = c(1000L, 800L),
  sir = c(0.10, 0.20),
  ifr = c(0.01, 0.05)
)
create_sample_linelist(
  strata = strata,
  times = 1L:2L,
  active_detection = 0.8,
  passive_asymptomatic_detection = 0.2,
  passive_symptomatic_detection = 0.6,
  force_of_infection = matrix(0.01, nrow = 2L, ncol = 2L)
)
#>    patient time    age detection      outcome
#> 1     UID1    1  Adult    Active Asymptomatic
#> 2     UID2    1  Adult    Active Asymptomatic
#> 3     UID3    1  Adult    Active Asymptomatic
#> 4     UID4    1  Adult    Active Asymptomatic
#> 5     UID5    1  Adult    Active Asymptomatic
#> 6     UID6    1  Adult    Active  Symptomatic
#> 7     UID7    2  Adult    Active Asymptomatic
#> 8     UID8    2  Adult    Active Asymptomatic
#> 9     UID9    2  Adult    Active Asymptomatic
#> 10   UID10    2  Adult    Active Asymptomatic
#> 11   UID11    2  Adult    Active Asymptomatic
#> 12   UID12    2  Adult    Active Asymptomatic
#> 13   UID13    1 Senior    Active Asymptomatic
#> 14   UID14    1 Senior    Active Asymptomatic
#> 15   UID15    1 Senior    Active Asymptomatic
#> 16   UID16    1 Senior    Active Asymptomatic
#> 17   UID17    1 Senior    Active Asymptomatic
#> 18   UID18    2 Senior    Active Asymptomatic
#> 19   UID19    2 Senior    Active Asymptomatic
#> 20   UID20    2 Senior    Active Asymptomatic
#> 21   UID21    2 Senior    Active Asymptomatic
#> 22   UID22    2 Senior    Active Asymptomatic
#> 23   UID23    2 Senior    Active Asymptomatic
#> 24   UID24    2 Senior    Active Asymptomatic
#> 25   UID25    2 Senior    Active  Symptomatic
#> 26   UID26    2 Senior    Active  Symptomatic
#> 27   UID27    2 Senior    Active  Symptomatic
#> 28   UID28    2  Adult   Passive Asymptomatic
#> 29   UID29    2  Adult   Passive  Symptomatic
#> 30   UID30    1 Senior   Passive Asymptomatic
#> 31   UID31    2 Senior   Passive Asymptomatic
```
