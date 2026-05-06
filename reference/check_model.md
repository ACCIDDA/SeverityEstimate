# Check a `SeverityEstimateModel` instance

Perform common checks on a `SeverityEstimateModel` instances, namely
that it is an instance of that class and has the attributes needed for
the particular function interacting with it.

## Usage

``` r
check_model(model, attribute = NULL, override_warning = TRUE)
```

## Arguments

- model:

  A
  [SeverityEstimateModel](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  S4 object instance representing a model to check.

- attribute:

  The attribute being modified by the function calling this check. If
  this attribute is set a warning will be issued to the user letting
  them know.

- override_warning:

  Whether there should be a warning if `attribute` is already set. This
  function only checks the length of the attribute to determine if it is
  set.

## Value

`check_model` returns `NULL` and will raise an error if there is an
issue.
