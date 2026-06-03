# Build Strata Basis Terms And Design Matrices

Helpers for turning ordered or categorical strata specifications into
basis terms for the
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
Stan model. `build_strata_level_basis()` constructs the per-dimension
basis matrix for one strata variable, while
`build_strata_design_matrix()` expands those terms across the full
strata-cell grid used by the model.

## Usage

``` r
build_strata_design_matrix(strata, strata_df)

build_strata_level_basis(levels, degrees_of_freedom)
```

## Arguments

- strata:

  A list of strata specifications from `strata(model)`.

- strata_df:

  The expanded strata-cell data frame returned by
  [`incidence_population_arrays()`](https://accidda.github.io/SeverityEstimate/reference/incidence_population_arrays.md).

- levels:

  The ordered levels for the dimension.

- degrees_of_freedom:

  The requested degrees of freedom.

## Value

`build_strata_design_matrix()` returns a named list with `X_strata`, the
additive design matrix, and `n_strata_basis_cols`, its column count.

`build_strata_level_basis()` returns a numeric matrix with one row per
level and one column per basis term.
