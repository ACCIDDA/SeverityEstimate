# Validate A Mapping Of Values From A Line List

Validate A Mapping Of Values From A Line List

## Usage

``` r
validate_map(model, name, map, valid_types, required_types = character())
```

## Arguments

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  S4 object instance representing a model to check.

- name:

  A column name to check for in either the `line_list` slot of `model`.

- map:

  A named character vector mapping column values to valid types.

- valid_types:

  A character vector of valid types that map values must be.

- required_types:

  A character vector of valid types that map values must include.

## Value

`NULL`, if there are no issues.
