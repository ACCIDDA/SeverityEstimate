# Infer Default Mappings For Formatted Model Inputs

Internal helpers used by
[`default_model()`](https://accidda.github.io/SeverityEstimate/reference/default_model.md)
to infer the detection and outcome mappings required by the model from
already-formatted line list values.

`infer_default_map()` is the generic validator and mapper. It lowercases
the observed values, checks that they can all be interpreted using a
supplied lookup table, and returns a named character vector from the
original raw values to the canonical model labels.

`infer_default_detection_map()` specializes this for the `detection`
column, accepting case-insensitive forms of `active`/`passive` and the
abbreviations `a`/`p`.

`infer_default_outcome_map()` specializes this for the `outcome` column,
accepting case-insensitive forms of `asymptomatic`/`symptomatic`/`death`
and the abbreviations `a`/`s`/`d`.

These helpers are not exported, but they are documented to make the
package's data assumptions explicit for contributors extending the
model-construction workflow.

## Usage

``` r
infer_default_map(values, valid_map, value_name, required_types = character())

infer_default_detection_map(values)

infer_default_outcome_map(values)
```

## Arguments

- values:

  A vector of observed raw values to map. For the specialized helpers,
  these are the raw values from the `detection` or `outcome` column.

- valid_map:

  A named character vector of lowercase input values to canonical model
  values.

- value_name:

  The user-facing name of the values being validated.

- required_types:

  Canonical values that must be represented in the inferred map.

## Value

A named character vector mapping raw values to canonical model values.
