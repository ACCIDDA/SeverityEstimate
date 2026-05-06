# Is `x` A 'data.frame'?

Checks if the given variable is a 'data.frame' like object and if it is
not exactly a 'data.frame' will convert it to one.

## Usage

``` r
is_data_frame(x, has_string_columns = character())
```

## Arguments

- x:

  The object to check and downcast.

- has_string_columns:

  A character vector of columns to check for the presence of as well as
  check it is a character/factor column.

## Value

Either `x` if it is only a 'data.frame' otherwise `x` downcasted as a
'data.frame'.
