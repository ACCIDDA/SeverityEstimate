#' @title
#' Estimate Severity From A Line List
#'
#' @description
#' Fit a severity estimate model accounting for under reporting of asymptomatic
#' and mildly symptomatic cases.
#'
#' @param linelist A `data.frame` (or `data.frame` extending object like a
#' `tibble`) of the line list data.
#' @param population A `data.frame` (or `data.frame` extending object like a
#' `tibble`) of the population data.
#' @param time_period A character of columns describing the time period such as
#' 'week' or 'day'
#' @param strata A character of columns describing the attributes to stratify
#' the data on.
# TODO: `surveillance` and `outcome` are special, need to add those details
#' @param surveillance A character of columns describing the surveillance
#' methods. The values in this column must be coercible to 'Active' and
#' 'Passive'.
#' @param outcome A character of columns describing the outcome of the line list
#' entry. The values in this column must be coercible to 'Asymptomatic',
#' 'Death', and 'Symptomatic'.
#' @param population_value A unit length character corresponding to the column
#' in the `population` data.frame (or data.frame like object) for the population
#' value.
#' @param surveillance_reference Either `NULL`, a `data.frame`, or a vector of
#' values to use as a reference for the surveillance dimension. If a vector then
#' it is expected that `surveillance` is unit length.
#' @param outcome_reference Either `NULL`, a `data.frame`, or a vector of
#' values to use as a reference for the outcome dimension. If a vector then
#' it is expected that `outcome` is unit length.
#' @param time_period_reference Either `NULL`, a `data.frame`, or a vector of
#' values to use as a reference for the time period dimension. If a vector then
#' it is expected that `time_period` is unit length.
#' @param strata_reference Either `NULL`, a `data.frame`, or a vector of
#' values to use as a reference for the strata dimension. If a vector then
#' it is expected that `strata` is unit length.
#' @param additional_betas_mean A single length numeric for the mean to use in
#' the strongly symptomatic prior distribution.
#' @param additional_betas_std A single length numeric for the standard
#' deviation to use in the strongly symptomatic prior distribution.
#' @param hazard_std A single length numeric greater than zero for the standard
#' deviation to use in the community hazard prior distribution.
#' @param degrees_of_freedom A single length integer greater than zero for the
#' degrees of freedom to use in the model.
#' @param ... Further optional args that are eventually given to [rstan::stan()]
#' related to fitting.
#'
#' @returns
#' A \linkS4class{SeverityEstimateFit} S4 object.
#'
#' @importFrom utils tail
#' @importFrom methods new
#' @export
estimate_severity <- function(
  linelist,
  population,
  surveillance,
  outcome,
  time_period = character(),
  strata = character(),
  population_value = utils::tail(names(population), n = 1L),
  surveillance_reference = NULL,
  outcome_reference = NULL,
  time_period_reference = NULL,
  strata_reference = NULL,
  additional_betas_mean = 0.0,
  additional_betas_std = 5.0,
  hazard_std = 3.0,
  degrees_of_freedom = 1L,
  ...
) {
  # Input validation
  stopifnot(
    is.data.frame(linelist),
    is.data.frame(population)
  )
  if (!setequal(class(linelist), "data.frame")) {
    linelist <- as.data.frame(linelist)
  }
  if (!setequal(class(population), "data.frame")) {
    population <- as.data.frame(population)
  }
  stopifnot(
    # Check on the surveillance column of line list
    is.character(surveillance),
    length(surveillance) == 1L,
    !is.na(surveillance),
    surveillance %in% names(linelist),
    is.factor(linelist[, surveillance]) ||
      is.character(linelist[, surveillance]),
    # Check on the outcome column of the line list
    is.character(outcome),
    length(outcome) == 1L,
    !is.na(outcome),
    outcome %in% names(linelist),
    is.factor(linelist[, outcome]) ||
      is.character(linelist[, outcome]),
    # Check on additional betas mean
    is.numeric(additional_betas_mean),
    length(additional_betas_mean) == 1L,
    !is.na(additional_betas_mean),
    # Check on additional betas standard deviation
    is.numeric(additional_betas_std),
    length(additional_betas_std) == 1L,
    !is.na(additional_betas_std),
    additional_betas_std > 0.0,
    # Check on hazard variance
    is.numeric(hazard_std),
    length(hazard_std) == 1L,
    !is.na(hazard_std),
    hazard_std > 0.0,
    # Check on degrees of freedom
    is.integer(degrees_of_freedom),
    length(degrees_of_freedom) == 1L,
    !is.na(degrees_of_freedom),
    degrees_of_freedom >= 0L
  )

  # Construct the incidents array
  arrays <- incidents_population_arrays(
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
  )
  surveillance_df <- format_surveillance_data_frame(arrays$surveillance)
  outcome_df <- format_outcome_data_frame(arrays$outcome)

  # Compile together the data given and format it
  surveillance_ind <- match(
    c("Active", "Passive", "Unknown"),
    surveillance_df[, surveillance]
  )
  outcome_ind <- match(
    c("Asymptomatic", "Symptomatic", "Death"), outcome_df[, outcome]
  )
  incidents_without_outcome <- rowSums(arrays$incidents, dims = 3L)
  active_ind <- which(
    arrays$linelist_ind[, "surveillance", drop = TRUE] == surveillance_ind[1L]
  )
  passive_ind <- which(
    arrays$linelist_ind[, "surveillance", drop = TRUE] == surveillance_ind[2L]
  )

  data <- list(
    strata_groups = nrow(arrays$strata),
    time_groups = nrow(arrays$time_period),
    I_passive = incidents_without_outcome[, , surveillance_ind[2L]],
    I_active = incidents_without_outcome[, , surveillance_ind[1L]],
    population = arrays$population,
    observed_active = length(active_ind),
    observed_passive = length(passive_ind),
    strata_active = arrays$linelist_ind[active_ind, "strata", drop = TRUE],
    symptoms_active = (
      arrays$linelist_ind[active_ind, "outcome", drop = TRUE]
      %in% outcome_ind[2L:3L]
    ),
    dead_active = (
      arrays$linelist_ind[active_ind, "outcome", drop = TRUE]
      %in% outcome_ind[3L]
    ),
    strata_passive = arrays$linelist_ind[passive_ind, "strata", drop = TRUE],
    symptoms_passive = (
      arrays$linelist_ind[passive_ind, "outcome", drop = TRUE]
      %in% outcome_ind[2L:3L]
    ),
    dead_passive = (
      arrays$linelist_ind[passive_ind, "outcome", drop = TRUE]
      %in% outcome_ind[3L]
    ),
    additional_betas_mean = additional_betas_mean,
    additional_betas_std = additional_betas_std,
    hazard_std = hazard_std,
    degrees_of_freedom = degrees_of_freedom
  )

  # Pass along everything to the model
  model_fit <- stan_model(
    "severity_estimate_by_surveillance.stan",
    data = data,
    ...
  )

  # Create and return severity fit
  severity_estimate_fit <- new(
    "SeverityEstimateFit",
    model_fit = model_fit,
    population = arrays$population,
    incidents = arrays$incidents,
    time_period = arrays$time_period,
    strata = arrays$strata,
    surveillance = surveillance_df,
    outcome = outcome_df
  )
  severity_estimate_fit
}
