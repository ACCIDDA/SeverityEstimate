# Internal Helpers For `SummaryEstimateFit`

Internal helpers used to construct and format `SummaryEstimateFit`
objects.

## Usage

``` r
new_summary_estimate_fit(detection_rates, severity_estimates)

format_summary_detection_rates(detection_rates)

format_summary_severity_estimates(severity_estimates)
```

## Arguments

- detection_rates:

  A `data.frame` of detection-rate estimates.

- severity_estimates:

  A `data.frame` of IFR/SIR estimates, optionally including strata
  columns.

## Value

`new_summary_estimate_fit` returns an object of class
`SummaryEstimateFit` with elements `detection_rates` and
`severity_estimates`.

`format_summary_detection_rates` returns a `data.frame` of mean
detection rate estimates formatted for printing.

`format_summary_severity_estimates` returns a `data.frame` of mean
IFR/SIR estimates with strata columns first and renamed estimate
columns.
