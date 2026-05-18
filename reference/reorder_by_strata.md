# Reorder A `data.frame` Using Strata Reference Rows

Reorder a `data.frame` so the strata columns follow the row order of a
reference strata `data.frame`.

## Usage

``` r
reorder_by_strata(x, strata)
```

## Arguments

- x:

  A `data.frame` containing the strata columns to reorder by.

- strata:

  A reference `data.frame` whose rows define the desired strata order.

## Value

A reordered `data.frame` with row names reset.
