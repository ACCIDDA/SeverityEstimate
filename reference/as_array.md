# Coerce Vectors To One-Dimensional Arrays

Internal helpers for coercing vectors to one-dimensional arrays, with an
optional conversion step before array construction.

## Usage

``` r
as_array(x, converter = function(x) x)

as_integer_array(x)

as_numeric_array(x)
```

## Arguments

- x:

  A vector-like object to convert to an array.

- converter:

  A function applied to `x` before constructing the array.

## Value

A one-dimensional array with length equal to `length(x)`.
