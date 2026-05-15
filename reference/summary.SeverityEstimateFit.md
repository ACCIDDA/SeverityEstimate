# Summary Method for `SeverityEstimateFit` Objects

Summarises a fitted severity estimate model by reporting mean detection
rate estimates and mean IFR/SIR estimates by strata.

## Usage

``` r
# S3 method for class 'SeverityEstimateFit'
summary(object, ...)
```

## Arguments

- object:

  An object of class `SeverityEstimateFit`.

- ...:

  Unused.

## Value

`summary.SeverityEstimateFit` returns an object of class
`SummaryEstimateFit` with elements `detection_rates` and
`severity_estimates`.
