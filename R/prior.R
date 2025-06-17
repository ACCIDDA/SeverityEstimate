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
#' @importFrom methods is
#' @export
#' @rdname prior
prior <- function(model, parameter, params) {
  stopifnot(methods::is(model, "SeverityEstimateModel"))
  checkmate::assert_choice(
    parameter,
    c("active", "passive_asymptomatic", "passive_symptomatic")
  )
  if (parameter == "active") {
    model@active_prior <- beta_parameterization(params)
  } else if (parameter == "passive_asymptomatic") {
    model@passive_asymptomatic_prior <- beta_parameterization(params)
  } else {
    model@passive_symptomatic_prior <- beta_parameterization(params)
  }
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
