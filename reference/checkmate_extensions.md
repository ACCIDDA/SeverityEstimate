# Checkmate Extensions

Convenience wrappers around checkmate for specific internal use cases.
Namely:

- `check_probability`/`assert_probability` are wrappers around
  [`checkmate::check_number()`](https://mllg.github.io/checkmate/reference/checkNumber.html)
  specifically for probabilities.

- `assert_bool` is a wrappeer around
  [`checkmate::check_false()`](https://mllg.github.io/checkmate/reference/checkFALSE.html)
  and
  [`checkmate::check_true()`](https://mllg.github.io/checkmate/reference/checkTRUE.html)
  for single length logicals.

## Usage

``` r
check_probability(x, na.ok = FALSE, null.ok = FALSE)

assert_probability(
  x,
  na.ok = FALSE,
  null.ok = FALSE,
  .var.name = checkmate::vname(x),
  add = NULL
)

assert_bool(x, na.ok = FALSE, .var.name = checkmate::vname(x))
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- na.ok:

  \[`logical(1)`\]  
  Are missing values allowed? Default is `FALSE`.

- null.ok:

  \[`logical(1)`\]  
  If set to `TRUE`, `x` may also be `NULL`. In this case only a type
  check of `x` is performed, all additional checks are disabled.

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).
