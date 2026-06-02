#' @title
#' Prepare Strata Inputs For Model Fitting
#'
#' @description
#' Internal helpers used by [fit()] before incidence and population arrays are
#' built. The Stan severity model requires every modeled strata cell to have a
#' positive denominator because the community hazard prior is centered on
#' observed passive incidence divided by the strata population. Strata cells
#' with zero population therefore cannot be passed through as ordinary modeled
#' cells.
#'
#' `filter_positive_population_strata()` removes zero-population strata from
#' the population data and from the explicit strata reference grid used to shape
#' the fitted arrays. Population rows are first aggregated over `strata_cols`,
#' so duplicated population rows are treated as a single strata cell and are
#' retained when their total population is positive. If no strata cell has a
#' positive population, the function errors because there is no valid fitted
#' model to construct.
#'
#' Line-list observations are not silently dropped. After identifying positive
#' population strata, the function checks that every observation belongs to a
#' retained strata cell. Observations in a zero-population cell, or in a cell
#' missing from the positive population strata, produce an early R error with
#' the offending strata values. This keeps impossible inputs out of Stan while
#' preserving the original incidence accounting.
#'
#' `strata_key()` creates stable row keys for comparing strata cells across
#' data frames. It is used only for exact equality checks within already
#' validated strata columns.
#'
#' `format_strata_values()` formats one or more strata rows for diagnostics.
#'
#' @param linelist A `data.frame` containing the observed line-list records.
#' It must contain all columns named in `strata_cols`.
#' @param population A `data.frame` containing population counts by strata. It
#' must contain all columns named in `strata_cols` and the column named by
#' `population_value`.
#' @param strata_cols A character vector naming the strata columns shared by
#' `linelist`, `population`, and `strata_reference`.
#' @param population_value A single string naming the population count column in
#' `population`.
#' @param strata_reference A `data.frame` containing the ordered strata cells
#' that should be available to `incidence_population_arrays()`. For configured
#' strata this is usually the full cross-product of user-specified strata
#' levels; for the no-strata case it contains the synthetic `.strata` cell.
#' @param x A `data.frame` containing strata columns.
#'
#' @return
#' `filter_positive_population_strata()` returns a named list with:
#'
#' * `population`: `population` restricted to rows whose aggregate strata
#' population is positive.
#' * `strata_reference`: `strata_reference` restricted to the same positive
#' strata cells, with its input row order preserved.
#'
#' `strata_key()` returns a character vector with one key per row in `x`.
#'
#' `format_strata_values()` returns a single diagnostic string describing the
#' strata values in `x`.
#'
#' @noRd
filter_positive_population_strata <- function(
  linelist,
  population,
  strata_cols,
  population_value,
  strata_reference
) {
  population_totals <- stats::aggregate(
    population[[population_value]],
    population[, strata_cols, drop = FALSE],
    sum
  )
  names(population_totals)[ncol(population_totals)] <- population_value

  positive_population <- population_totals[[population_value]] > 0
  if (!any(positive_population)) {
    stop(
      "At least one strata group must have a positive population.",
      call. = FALSE
    )
  }

  positive_strata <- population_totals[
    positive_population,
    strata_cols,
    drop = FALSE
  ]
  positive_keys <- strata_key(positive_strata, strata_cols)

  linelist_keys <- strata_key(linelist, strata_cols)
  linelist_in_positive_strata <- linelist_keys %in% positive_keys
  if (!all(linelist_in_positive_strata)) {
    invalid_strata <- unique(linelist[
      !linelist_in_positive_strata,
      strata_cols,
      drop = FALSE
    ])
    stop(
      "The line list contains observations in strata groups with zero or ",
      "missing population: ",
      format_strata_values(invalid_strata),
      ".",
      call. = FALSE
    )
  }

  population_keys <- strata_key(population, strata_cols)
  strata_reference_keys <- strata_key(strata_reference, strata_cols)

  population <- population[population_keys %in% positive_keys, , drop = FALSE]
  strata_reference <- strata_reference[
    strata_reference_keys %in% positive_keys,
    ,
    drop = FALSE
  ]
  rownames(population) <- NULL
  rownames(strata_reference) <- NULL

  list(
    population = population,
    strata_reference = strata_reference
  )
}

#' @noRd
strata_key <- function(x, strata_cols) {
  do.call(
    paste,
    c(lapply(x[, strata_cols, drop = FALSE], as.character), sep = "\r")
  )
}

#' @noRd
format_strata_values <- function(x) {
  values <- apply(
    x,
    1L,
    function(row) paste(names(row), row, sep = "=", collapse = ", ")
  )
  toString(values)
}
