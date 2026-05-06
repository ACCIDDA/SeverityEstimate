# Extract And Match A Subset Of A data.frame

Extract a data.frame that is a unique subset of the given data.frame for
the columns and get the index of those rows in the subset.

## Usage

``` r
extract_and_match_data_frame(x, cols, subset_x = NULL, stop_on_nomatch = TRUE)
```

## Arguments

- x:

  A data.frame to extract and match from.

- cols:

  A character of columns to extract and match.

- subset_x:

  An optional data.frame that is a subset of `x` to use for extracting.
  If `NULL` then is computed from `x`.

- stop_on_nomatch:

  A single logical indicating if an error should be thrown in the event
  of a no match. This can only happen when `subset_x` is given and does
  not cover the values in `x`.

## Value

A list with two named elements, the first is 'df' is the data.frame of
the extracted subset and 'ind' is the match index of `x` in 'df'.
