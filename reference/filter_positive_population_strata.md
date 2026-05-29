# Prepare Strata Inputs For Model Fitting

Internal helpers used by
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
before incidence and population arrays are built. The Stan severity
model requires every modeled strata cell to have a positive denominator
because the community hazard prior is centered on observed passive
incidence divided by the strata population. Strata cells with zero
population therefore cannot be passed through as ordinary modeled cells.

`filter_positive_population_strata()` removes zero-population strata
from the population data and from the explicit strata reference grid
used to shape the fitted arrays. Population rows are first aggregated
over `strata_cols`, so duplicated population rows are treated as a
single strata cell and are retained when their total population is
positive. If no strata cell has a positive population, the function
errors because there is no valid fitted model to construct.

Line-list observations are not silently dropped. After identifying
positive population strata, the function checks that every observation
belongs to a retained strata cell. Observations in a zero-population
cell, or in a cell missing from the positive population strata, produce
an early R error with the offending strata values. This keeps impossible
inputs out of Stan while preserving the original incidence accounting.

`strata_key()` creates stable row keys for comparing strata cells across
data frames. It is used only for exact equality checks within already
validated strata columns.

`format_strata_values()` formats one or more strata rows for
diagnostics.

## Usage

``` r
filter_positive_population_strata(
  linelist,
  population,
  strata_cols,
  population_value,
  strata_reference
)

strata_key(x, strata_cols)

format_strata_values(x)
```

## Arguments

- linelist:

  A `data.frame` containing the observed line-list records. It must
  contain all columns named in `strata_cols`.

- population:

  A `data.frame` containing population counts by strata. It must contain
  all columns named in `strata_cols` and the column named by
  `population_value`.

- strata_cols:

  A character vector naming the strata columns shared by `linelist`,
  `population`, and `strata_reference`.

- population_value:

  A single string naming the population count column in `population`.

- strata_reference:

  A `data.frame` containing the ordered strata cells that should be
  available to
  [`incidence_population_arrays()`](https://accidda.github.io/SeverityEstimate/reference/incidence_population_arrays.md).
  For configured strata this is usually the full cross-product of
  user-specified strata levels; for the no-strata case it contains the
  synthetic `.strata` cell.

- x:

  A `data.frame` containing strata columns.

## Value

`filter_positive_population_strata()` returns a named list with:

- `population`: `population` restricted to rows whose aggregate strata
  population is positive.

- `strata_reference`: `strata_reference` restricted to the same positive
  strata cells, with its input row order preserved.

`strata_key()` returns a character vector with one key per row in `x`.

`format_strata_values()` returns a single diagnostic string describing
the strata values in `x`.
