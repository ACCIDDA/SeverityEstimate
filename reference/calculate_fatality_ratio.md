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

## Value

`calculate_fatality_ratio.SeverityEstimateFit` returns a `data.frame`
describing fatality ratios by strata or if now strata were provided to
when fitting a single row `data.frame`.

`calculate_fatality_ratio.default` signals an error.
