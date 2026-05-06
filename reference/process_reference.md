# Process A Vector Or `data.frame` Of Reference Values

Convert a given set of values into a formatted `data.frame` for
reference.

## Usage

``` r
process_reference(values, columns)
```

## Arguments

- values:

  Either `NULL`, a `data.frame`, or a vector to convert into a
  `data.frame`.

- columns:

  The expected columns of the output reference `data.frame`. If `values`
  is a vector then this can only be a single length vector.

## Value

If `values` is `NULL` then `NULL` otherwise a `data.frame` with the
columns specified by `columns`.
