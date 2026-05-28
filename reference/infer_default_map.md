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
abbreviations `a`/`p`. If only one detection type has been observed, the
returned map is completed with the corresponding unobserved
active/passive level so sparse early-outbreak data can still create both
surveillance dimensions.

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

complete_default_detection_map(map)

default_detection_level(type, existing_levels)

match_detection_case(value, template)
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

- map:

  A named character vector mapping raw detection values to canonical
  `active`/`passive` labels.

- type:

  A canonical detection type to create an unobserved raw label for.

- existing_levels:

  Existing raw detection levels whose style should be used when creating
  the missing level.

- value:

  A detection label whose case should be adjusted.

- template:

  A detection label that provides the desired case style.

## Value

- `infer_default_map()`, `infer_default_detection_map()`,
  `infer_default_outcome_map()`, and `complete_default_detection_map()`
  return a named character vector mapping raw values to canonical model
  values.

- `default_detection_level()` returns a length-one character vector
  containing the inferred raw label for an unobserved detection type.

- `match_detection_case()` returns `value` converted to match the case
  style of `template`.
