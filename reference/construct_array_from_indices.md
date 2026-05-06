# Create An Array From Vectors Of Indices And Values

Create an array from vectors of indices, such as (i,j,k) notation, with
optional values and dimnames for the output array.

## Usage

``` r
construct_array_from_indices(..., target = 1L, dim = NULL, dimnames = NULL)
```

## Arguments

- ...:

  Indices given either as an arbitrary number of equal length numerics
  or as a single list containing an arbitrary number of equal length
  numerics.

- target:

  The value to populate the array values with. This parameter will be
  coerced to match the length of the indices given.

- dim:

  The dim attribute for the array to be created (like
  [`base::array()`](https://rdrr.io/r/base/array.html)) or if `NULL`
  will be inferred from the max index value along each dimension
  provided for `...`. If not `NULL` then the length must match that of
  `...`.

- dimnames:

  Either a named list with corresponding dimname vectors (like
  [`base::array()`](https://rdrr.io/r/base/array.html)), a character
  vector to just name the dimnames and use a default sequence for the
  dimname values, or `NULL` for not assigning dimnames to the output.

## Value

A array with the same number of dimensions as given by `...` populated
with the values from `target`.

## Details

Overlapping indices will be summed. For example if this function was
given `c(1L, 1L)` and `c(1L, 1L)` for `...` then the resulting array
would be 1 x 1 with the entry being `2L`.

When given `NULL` or a list for `dimnames` that argument has the same
behavior as [`base::array()`](https://rdrr.io/r/base/array.html).
However, this function also provides the ability to just provide the
dimname names as a character and populate the dimnames values with a
sequence (`1L:N`). This third option for the `dimnames` argument is for
situations where the output array dimensions is not known in advance
without taking the `max` of each index vector given.
