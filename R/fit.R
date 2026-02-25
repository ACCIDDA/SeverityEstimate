#' @title
#' Fit a severity estimate model instance.
#'
#' @param model A \linkS4class{SeverityEstimateModel} to specify the prior
#' distribution for.
#' @param ... Further optional args that are eventually given to [rstan::stan()]
#' related to fitting.
#'
#' @export
fit <- function(model, ...) {
  check_model(model)

  # Extract prior parameterizations
  active_prior <- detection_prior("active", model@active_prior)
  passive_asymptomatic_prior <- detection_prior(
    "passive asymptomatic",
    model@passive_asymptomatic_prior
  )
  passive_symptomatic_prior <- detection_prior(
    "passive asymptomatic",
    model@passive_symptomatic_prior
  )

  # Construct inputs to fit
  data <- list(
    active_prior_alpha = active_prior[["alpha"]],
    active_prior_beta = active_prior[["beta"]],
    passive_asymptomatic_prior_alpha = passive_asymptomatic_prior[["alpha"]],
    passive_asymptomatic_prior_beta = passive_asymptomatic_prior[["beta"]],
    passive_symptomatic_prior_alpha = passive_symptomatic_prior[["alpha"]],
    passive_symptomatic_prior_beta = passive_symptomatic_prior[["beta"]]
  )

  # Fit
  model_fit <- stan_model(
    "estimate_severity.stan",
    data = data,
    ...
  )

  NULL
}
