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

## Examples

``` r
strata_spec <- list(
  list(
    name = "age",
    levels = c("Youth", "Adult", "Senior"),
    degrees_of_freedom = 1L
  ),
  list(
    name = "region",
    levels = c("North", "South"),
    degrees_of_freedom = 0L
  )
)
strata_df <- expand.grid(
  age = c("Youth", "Adult", "Senior"),
  region = c("North", "South"),
  stringsAsFactors = FALSE
)
# jarl-ignore internal_function: example
SeverityEstimate:::build_strata_design_matrix(strata_spec, strata_df)
#> $X_strata
#>                age_1   region_1
#> Youth  -1.000000e+00  0.7071068
#> Adult  -2.168241e-17  0.7071068
#> Senior  1.000000e+00  0.7071068
#> Youth  -1.000000e+00 -0.7071068
#> Adult  -2.168241e-17 -0.7071068
#> Senior  1.000000e+00 -0.7071068
#> 
#> $n_strata_basis_cols
#> [1] 2
#> 

# jarl-ignore internal_function: example
SeverityEstimate:::build_strata_level_basis(
  c("Youth", "Adult", "Senior"),
  degrees_of_freedom = 0L
)
#>        [,1] [,2]
#> Youth     1    0
#> Adult     0    1
#> Senior   -1   -1
#> attr(,"scaled:center")
#> [1] 0 0
#> attr(,"scaled:scale")
#> [1] 1 1
# jarl-ignore internal_function: example
SeverityEstimate:::build_strata_level_basis(
  c("Youth", "Adult", "Senior", "Elderly"),
  degrees_of_freedom = 2L
)
#>                  1          2
#> Youth   -1.1618950  0.8660254
#> Adult   -0.3872983 -0.8660254
#> Senior   0.3872983 -0.8660254
#> Elderly  1.1618950  0.8660254
#> attr(,"scaled:center")
#> 1 2 
#> 0 0 
#> attr(,"scaled:scale")
#>         1         2 
#> 0.5773503 0.5773503 
```
