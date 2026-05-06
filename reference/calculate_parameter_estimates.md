# Calculate Key Parameter Estimates

Calculate a summary `data.frame` describing the key parameters fitted
including:

- The mildly/asymptomatic passive detection rate,

- The severe symptoms passive detection rate, and

- The active detection rate.

## Usage

``` r
calculate_parameter_estimates(x, ...)

# S3 method for class 'SeverityEstimateFit'
calculate_parameter_estimates(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  include_description = TRUE,
  ...
)

# Default S3 method
calculate_parameter_estimates(x, ...)
```

## Arguments

- x:

  A object to calculate fatality ratio statistics from, typically a
  [SeverityEstimateFit](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
  S4 object.

- ...:

  Further arguments passed to other methods.

- mean_estimate:

  A single logical indicating if the mean estimate for the parameters
  should be included in the 'mean_estimate' column of the returned
  `data.frame`.

- median_estimate:

  A single logical indicating if the median estimate for the parameters
  should be included in the 'median_estimate column of the returned
  `data.frame`.

- alpha:

  A numeric of significance levels to return the parameters confidence
  intervals for. The columns will be in '{lower`/`upper}`_`{alpha}'
  format (i.e. 'lower`_`05' and 'upper`_`05' for `alpha=0.05`).

- include_description:

  A single logical indicating if descriptions of the key parameters
  should be included in the return value. Setting this to `TRUE` is
  handy for ad-hoc work, but setting to `FALSE` can be better for
  production code. The description will be given in the
  'parameter_description' column of the returned `data.frame`.

## Value

`calculate_parameter_estimates.SeverityEstimateFit` returns a
`data.frame` with the column 'parameter'. Other columns are determined
by other parameters, see above for details.

`calculate_fatality_ratio.default` signals an error.
