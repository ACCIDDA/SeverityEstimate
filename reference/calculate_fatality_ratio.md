# Calculate Fatality Ratio Statistics

Calculate a `data.frame` of fatality ratios broken down by strata, as
well as other optional model metrics.

## Usage

``` r
calculate_fatality_ratio(x, ...)

# S3 method for class 'SeverityEstimateFit'
calculate_fatality_ratio(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  naive_estimate = FALSE,
  alpha = 0.05,
  ...
)

# S3 method for class 'list'
calculate_fatality_ratio(
  x,
  strata,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  naive_estimate = FALSE,
  alpha = 0.05,
  incidence = NULL,
  outcome = NULL,
  ...
)

# Default S3 method
calculate_fatality_ratio(x, ...)
```

## Arguments

- x:

  A object to calculate fatality ratio statistics from, typically a
  [SeverityEstimateFit](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
  S4 object.

- ...:

  Further arguments passed to other methods.

- mean_estimate:

  A single logical indicating if the mean estimate for the ratios should
  be included in the '`*_`mean`_`estimate' columns of the returned
  `data.frame`.

- median_estimate:

  A single logical indicating if the median estimate for the ratios
  should be included in the '`*_`median`_`estimate column of the
  returned `data.frame`.

- naive_estimate:

  A single logical indicating if the naive estimate for fatality ratio
  should be included in the 'naive`_`estimate' column.

- alpha:

  A numeric of significance levels to return the parameters confidence
  intervals for. The columns will be in '`*_`{lower`/`upper}`_`{alpha}'
  format (i.e. '`*_`lower`_`05' and '`*_`upper`_`05' for `alpha=0.05`).

- strata:

  A `data.frame` describing the strata rows to report. For the list
  method, these rows are used as the reference strata for the returned
  estimates.

- incidence:

  An optional incidence array used to calculate naive fatality ratio
  estimates. This is required when `naive_estimate = TRUE`.

- outcome:

  An optional outcome `data.frame` used to map the incidence array
  columns when calculating naive fatality ratio estimates. This is
  required when `naive_estimate = TRUE`.

## Value

`calculate_fatality_ratio.SeverityEstimateFit` returns a `data.frame`
describing fatality ratios by strata or, if no strata were provided when
fitting a single row `data.frame`.

`calculate_fatality_ratio.default` signals an error.

## Examples

``` r
draws <- list(
  C = matrix(1, nrow = 4L, ncol = 2L),
  mortality = matrix(c(0.01, 0.02, 0.03, 0.04, 0.02, 0.03, 0.04, 0.05),
    nrow = 4L
  ),
  xi = matrix(c(0.08, 0.10, 0.12, 0.14, 0.18, 0.20, 0.22, 0.24),
    nrow = 4L
  )
)
strata <- data.frame(age = c("Adult", "Senior"))
calculate_fatality_ratio(draws, strata = strata, alpha = numeric())
#>      age ifr_mean_estimate ifr_median_estimate sir_mean_estimate
#> 1  Adult             0.025               0.025              0.11
#> 2 Senior             0.035               0.035              0.21
#>   sir_median_estimate
#> 1                0.11
#> 2                0.21
```
