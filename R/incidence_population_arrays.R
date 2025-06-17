#' @title Convert Incidence And Populations To Like Shaped Arrays
#'
#' @description
#' Create an incidence and population arrays that are compatible in shape from
#' a data.frame like line list and population.
#'
#' @param linelist A line list to convert into an array.
#' @param population Population data set to convert into an array.
#' @param time_period A character of columns describing the time period such as
#' 'week' or 'day'
#' @param strata A character of columns describing the attributes to stratify
#' the data on.
#' @param surveillance A character of columns describing the surveillance
#' methods.
#' @param outcome A character of columns describing the outcome of the line list
#' entry.
#' @param population_value A unit length character corresponding to the column
#' in the `population` data.frame (or data.frame like object) for the population
#' value.
#'
#' @details
#' It is expected that all of the `time_period`, `strata`, `surveillance`, and
#' `outcome` columns are all present in `linelist`. All of the `strata` columns
#' must be in `population`.
#'
#' @return
#' A list with names 'incidence', 'population', 'time_period', 'strata',
#' 'surveillance', and 'outcome'. These named list elements correspond to:
#' * `incidence`: A four dimensional array with the dimensions referring to
#' `time_period`, `strata`, `surveillance`, and `outcome`.
#' * `population`: A one dimensional array with the dimensions referring to
#' `strata`.
#' * `time_period`: A data.frame constructed from the `time_period` columns
#' given and whose rownames correspond to the `time_period` dimension.
#' * `strata`: A data.frame constructed from the `strata` columns given and
#' whose rownames correspond to the `strata` dimension.
#' * `surveillance`: A data.frame constructed from the `surveillance` columns
#' given and whose rownames correspond to the `surveillance` dimension.
#' * `outcome`: A data.frame constructed from the `outcome` columns given and
#' whose rownames correspond to the `outcome` dimension.
#' * `linelist_ind`: An integer matrix with the same number of rows as
#' `linelist` and columns corresponding to the dimensions extracted. The values
#' indicate where each line list element corresponds to in each of those
#' dimensions.
#'
#' The strata dimensions of `incidence` and `population` will be the same.
#'
#' @importFrom utils tail
#' @keywords internal
incidence_population_arrays <- function(
  linelist,
  population,
  time_period,
  strata,
  surveillance,
  outcome,
  population_value,
  time_period_reference,
  strata_reference,
  surveillance_reference,
  outcome_reference
) {
  # Downcast to data.frame for simplicity
  if (!setequal(class(linelist), "data.frame")) {
    linelist <- as.data.frame(linelist)
  }
  if (!setequal(class(population), "data.frame")) {
    population <- as.data.frame(population)
  }

  # Input validation
  stopifnot(
    all(time_period %in% names(linelist)),
    all(strata %in% names(linelist)),
    all(surveillance %in% names(linelist)),
    all(outcome %in% names(linelist)),
    all(strata %in% names(population)),
    length(population_value) == 1L,
    population_value %in% names(population)
  )
  time_period_reference <- process_reference(time_period_reference, time_period)
  strata_reference <- process_reference(strata_reference, strata)
  surveillance_reference <- process_reference(
    surveillance_reference,
    surveillance
  )
  outcome_reference <- process_reference(outcome_reference, outcome)

  # Time period/surveillance/outcome are only in `linelist` so start there
  extract_list <- extract_and_match_data_frame(
    linelist,
    time_period,
    subset_x = time_period_reference
  )
  extract_time_period <- extract_list$df
  linelist_time_period_ind <- extract_list$ind

  extract_list <- extract_and_match_data_frame(
    linelist,
    surveillance,
    subset_x = surveillance_reference
  )
  extract_surveillance <- extract_list$df
  linelist_surveillance_ind <- extract_list$ind

  extract_list <- extract_and_match_data_frame(
    linelist,
    outcome,
    subset_x = outcome_reference
  )
  extract_outcome <- extract_list$df
  linelist_outcome_ind <- extract_list$ind

  # Extract strata
  extract_df <- strata_reference
  if (is.null(strata_reference)) {
    extract_df <- unique(
      rbind(
        unique(linelist[, strata, drop = FALSE]),
        unique(population[, strata, drop = FALSE])
      )
    )
  }
  extract_list <- extract_and_match_data_frame(
    population,
    strata,
    subset_x = extract_df
  )
  extract_strata <- extract_list$df
  population_strata_ind <- extract_list$ind

  linelist_strata_ind <- match(
    interaction(linelist[, strata]),
    interaction(extract_strata)
  )

  linelist_extra_strata_ind <- setdiff(
    linelist_strata_ind,
    population_strata_ind
  )
  if (length(linelist_extra_strata_ind) > 0L) {
    stop(
      "There are `strata` values found in the line ",
      "list that are not found in the population."
    )
  }

  # Build the `incidence` and `population` arrays
  population <- construct_array_from_indices(
    population_strata_ind,
    target = population[, population_value],
    dim = nrow(extract_strata),
    dimnames = "strata"
  )
  incidence <- construct_array_from_indices(
    linelist_time_period_ind,
    linelist_strata_ind,
    linelist_surveillance_ind,
    linelist_outcome_ind,
    dim = c(
      nrow(extract_time_period),
      nrow(extract_strata),
      nrow(extract_surveillance),
      nrow(extract_outcome)
    ),
    dimnames = c("time_period", "strata", "surveillance", "outcome")
  )

  # Build the indices matrix
  linelist_ind <- cbind(
    linelist_time_period_ind,
    linelist_strata_ind,
    linelist_surveillance_ind,
    linelist_outcome_ind
  )
  dimnames(linelist_ind) <- list(
    "row" = seq_along(linelist_time_period_ind),
    "dimension" = c("time_period", "strata", "surveillance", "outcome")
  )

  # Done
  list(
    "incidence" = incidence,
    "population" = population,
    "time_period" = extract_time_period,
    "strata" = extract_strata,
    "surveillance" = extract_surveillance,
    "outcome" = extract_outcome,
    "linelist_ind" = linelist_ind
  )
}
