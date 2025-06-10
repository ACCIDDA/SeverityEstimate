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
#' @param hazard_std A single length numeric greater than zero for the standard
#' deviation to use in the community hazard prior distribution.
#' @param degrees_of_freedom A single length integer greater than zero for the
#' degrees of freedom to use in the model.
#' @param active_prior The parameters for the prior beta distribution for the
#' active detection probability. Can be specified as 'alpha'/'beta',
#' 'mean'/'var', or 'mean'/'sd'.
#' @param passive_asymptomatic_prior The parameters for the prior beta
#' distribution for the passive asymptomatic detection probability. Can be
#' specified as 'alpha'/'beta', 'mean'/'var', or 'mean'/'sd'.
#' @param passive_symptomatic_prior The parameters for the prior beta
#' distribution for the passive symptomatic detection probability. Can be
#' specified as 'alpha'/'beta', 'mean'/'var', or 'mean'/'sd'.
#' @param ... Further optional args that are eventually given to [rstan::stan()]
#' related to fitting.
#'
#' @returns
#' A \linkS4class{SeverityEstimateFit} S4 object.
#'
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_number
#' @importFrom checkmate assert_string
#' @importFrom methods new
#' @importFrom utils tail
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
  hazard_std = 3.0,
  degrees_of_freedom = 1L,
  active_prior = c("alpha" = 1.0, "beta" = 25.0),
  passive_asymptomatic_prior = c("alpha" = 1.0, "beta" = 2.0),
  passive_symptomatic_prior = c("alpha" = 25.0, "beta" = 1.0),
  ...
) {
  # Input validation
  checkmate::assert_string(surveillance)
  checkmate::assert_string(outcome)
  linelist <- is_data_frame(
    linelist, has_string_columns = c(surveillance, outcome)
  )
  population <- is_data_frame(population)
  # Check on hazard variance
  checkmate::assert_number(
    hazard_std, lower = .Machine$double.eps, finite = TRUE
  )
  # Check on degrees of freedom
  checkmate::assert_integerish(degrees_of_freedom, lower = 0L)
  degrees_of_freedom <- as.integer(degrees_of_freedom)
  # Checks on prior parameters
  active_prior <- beta_parameterization(active_prior)
  passive_asymptomatic_prior <- beta_parameterization(
    passive_asymptomatic_prior
  )
  passive_symptomatic_prior <- beta_parameterization(passive_symptomatic_prior)
  # Construct the incidence array
  arrays <- incidence_population_arrays(
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
  incidence_without_outcome <- rowSums(arrays$incidence, dims = 3L)
  active_ind <- which(
    arrays$linelist_ind[, "surveillance", drop = TRUE] == surveillance_ind[1L]
  )
  passive_ind <- which(
    arrays$linelist_ind[, "surveillance", drop = TRUE] == surveillance_ind[2L]
  )
  data <- list(
    strata_groups = nrow(arrays$strata),
    time_groups = nrow(arrays$time_period),
    I_passive = incidence_without_outcome[, , surveillance_ind[2L]],
    I_active = incidence_without_outcome[, , surveillance_ind[1L]],
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
    hazard_std = hazard_std,
    degrees_of_freedom = degrees_of_freedom,
    active_detection_alpha = active_prior["alpha"],
    active_detection_beta = active_prior["beta"],
    passive_asymptomatic_alpha = passive_asymptomatic_prior["alpha"],
    passive_asymptomatic_beta = passive_asymptomatic_prior["beta"],
    passive_symptomatic_alpha = passive_symptomatic_prior["alpha"],
    passive_symptomatic_beta = passive_symptomatic_prior["beta"]
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
    incidence = arrays$incidence,
    time_period = arrays$time_period,
    strata = arrays$strata,
    surveillance = surveillance_df,
    outcome = outcome_df
  )
  severity_estimate_fit
}
