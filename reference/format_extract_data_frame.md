# Format And Validate A Extract `data.frame`s

Format the labels of a single column surveillance/outcome `data.frame`
for internal use.

## Usage

``` r
format_extract_data_frame(
  df,
  name,
  valid_levels,
  level_converter = identity,
  case_insensitive_levels = TRUE
)

format_surveillance_data_frame(df)

format_outcome_data_frame(df)
```

## Arguments

- df:

  A `data.frame` (or `data.frame` extending object like a `tibble`)
  describing the levels.

- name:

  A length one character of a name to provide for error messages.

- valid_levels:

  A list of characters describing sets of valid levels. Each character
  in this list should be the same length.

- level_converter:

  A function to process the values in `df` before converting to a
  factor.

- case_insensitive_levels:

  A length one logical indicating if levels are case insensitive.

## Value

A `data.frame` that is similar to the input given, but with a single
column as a factor with:

- 'Active' and 'Passive' levels for `format_surveillance_data_frame`,
  and

- 'Asymptomatic', 'Death', and 'Symptomatic' levels for
  `format_outcome_data_frame`.
