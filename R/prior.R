#' @title
#' Specify Prior Parameterization For A Model Parameter
#'
#' @param model A \linkS4class{SeverityEstimateModel} to specify the prior
#' distribution for.
#' @param parameter The model parameter to specify the prior for, should be one
#' of 'active', 'passive_asymptomatic', 'passive_symptomatic'.
#' @inheritParams beta_parameterization
#'
#' @return
#' The `model` given modified to contain the prior parameterization for
#' `parameter`.
#'
#' @importFrom checkmate assert_choice
#' @importFrom methods slot<-
#' @export
#' @rdname prior
prior <- function(model, parameter, params) {
  checkmate::assert_choice(
    parameter,
    c("active", "passive_asymptomatic", "passive_symptomatic")
  )
  parameter_prior <- paste0(parameter, "_prior")
  check_model(model, attribute = parameter_prior)
  slot(model, parameter_prior) <- beta_parameterization(params)
  model
}

#' @rdname prior
#' @export
active_prior <- function(model, params) {
  prior(model, "active", params)
}

#' @rdname prior
#' @export
passive_asymptomatic_prior <- function(model, params) {
  prior(model, "passive_asymptomatic", params)
}

#' @rdname prior
#' @export
passive_symptomatic_prior <- function(model, params) {
  prior(model, "passive_symptomatic", params)
}
