# Get Or Set Model Timesteps

S4 getter and replacement methods for the `timesteps` slot on a
[SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md),
plus a chainable `set_timesteps()` helper for pipeline ergonomics.

## Usage

``` r
timesteps(x)

timesteps(x) <- value

# S4 method for class 'SeverityEstimateModel'
timesteps(x)

has_timesteps(x)

# S4 method for class 'SeverityEstimateModel'
has_timesteps(x)

# S4 method for class 'SeverityEstimateModel'
timesteps(x) <- value

require_timesteps(model, mode = "error")

set_timesteps(model, name, levels = NULL)
```

## Arguments

- x:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- value:

  A named list with entries `name` and `levels`.

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md).

- mode:

  How `require_timesteps()` should respond when timesteps are not set.
  One of `error`, `warn`, `silent`.

- name:

  The name of the timestep column, which must be present in `line_list`.

- levels:

  The levels for the timestep, or `NULL` to infer from `line_list`.

## Value

`timesteps(x)` returns the current timestep specification.

`timesteps(x) <- value` returns `x` modified to include the timestep
specification.

`has_timesteps(x)` returns `TRUE` if timesteps have been set, `FALSE`
otherwise.

`require_timesteps(model, mode)` returns `model`. If timesteps are
unset, the behavior depends on `mode`.

`set_timesteps(model, ...)` returns `model` modified to include the
timestep specification.
