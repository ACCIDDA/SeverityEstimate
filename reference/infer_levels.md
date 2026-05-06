# Infer And Validate Levels For Variables

Infer And Validate Levels For Variables

## Usage

``` r
infer_levels(model, name, name_in, levels = NULL, ordered = FALSE)
```

## Arguments

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  S4 object instance representing a model to check.

- name:

  A column name to check for in either the `line_list` and/or
  `population` slots of `model`.

- name_in:

  A string indication which attribute to check `name` against. Must be
  'line_list', 'population', or 'both'.

- levels:

  Explicit user provided levels, in order, if provided otherwise `NULL`
  to infer the levels.

- ordered:

  A boolean indicating if `levels` has a specific order. If `TRUE` then
  `levels` cannot be `NULL`, users must explicitly indicate what the
  ordering is.

## Value

`infer_levels` returns a vector of levels for the particular column
given. Either `levels` if non-`NULL` or an inferred set of levels.
