# Calculate Hazard Statistics

Calculate a `data.frame` of posterior infection-hazard estimates broken
down by time period and strata.

## Usage

``` r
calculate_hazard(x, ...)

# S3 method for class 'SeverityEstimateFit'
calculate_hazard(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  ...
)

# S3 method for class 'list'
calculate_hazard(
  x,
  time_period,
  strata,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  ...
)

# Default S3 method
calculate_hazard(x, ...)
```

## Arguments

- x:

  An object to calculate hazard statistics from, typically a
  [SeverityEstimateFit](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
  S4 object.

- ...:

  Further arguments passed to other methods.

- mean_estimate:

  A single logical indicating if the mean estimate for the hazard should
  be included in the `mean_estimate` column of the returned
  `data.frame`.

- median_estimate:

  A single logical indicating if the median estimate for the hazard
  should be included in the `median_estimate` column of the returned
  `data.frame`.

- alpha:

  A numeric of significance levels to return the hazard confidence
  intervals for. The columns will be in `\{lower/upper\}_\{alpha\}`
  format (i.e. `lower_05` and `upper_05` for `alpha=0.05`).

- time_period:

  A `data.frame` describing the time-period dimension of the underlying
  hazard.

- strata:

  A `data.frame` describing the strata dimension of the underlying
  hazard.

## Value

`calculate_hazard.SeverityEstimateFit` returns a `data.frame` describing
posterior hazard estimates by time period and strata.

`calculate_hazard.default` signals an error.
