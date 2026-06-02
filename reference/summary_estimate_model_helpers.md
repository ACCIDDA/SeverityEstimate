# Internal Helpers For `SummaryEstimateModel`

Internal helpers used to construct and format `SummaryEstimateModel`
objects.

## Usage

``` r
new_summary_estimate_model(data, priors, timesteps, detection, outcome, strata)

format_summary_model_data(model)

format_summary_model_priors(model)

format_summary_model_timesteps(model)

format_summary_model_detection(model)

format_summary_model_outcome(model)

format_summary_model_map(model, specification, valid_types)

format_summary_model_strata(model)

format_summary_model_number(x, digits = 3L)

print_summary_model_map(x)
```

## Arguments

- data:

  A `data.frame` summarising model input data dimensions.

- priors:

  A `data.frame` summarising beta priors.

- timesteps:

  A `data.frame` summarising the timestep specification.

- detection:

  A `data.frame` summarising the detection mapping and counts.

- outcome:

  A `data.frame` summarising the outcome mapping and counts.

- strata:

  A `data.frame` summarising strata specifications.

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- specification:

  A model mapping specification.

- valid_types:

  Canonical model types represented in a mapping.

- x:

  A numeric vector to format or mapping summary `data.frame` to print.

- digits:

  The number of significant digits to keep.

## Value

`new_summary_estimate_model` returns an object of class
`SummaryEstimateModel`.
