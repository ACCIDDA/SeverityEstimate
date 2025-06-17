#' @title
#' Specify Prior Parameterization For A Model Parameter
#'
#' @param model A \linkS4class{SeverityEstimateModel} to specify the prior
#' distribution for.
#' @param parameter The model parameter to specify the prior for, should be one
#' of 'active', 'passive_asymptomatic', 'passive_symptomatic'.
#' @param ... Beta distribution parameterization. Must be one of 'alpha'/'beta',
#' 'mean'/'var', 'mean'/'sd'.
#'
#' @return
#' The `model` given modified to contain the prior parameterization for
#' `parameter`.
#'
#' @examples
#' line_list <- data.frame(
#'   patient = 1L:3L,
#'   week = c(1L, 1L, 2L),
#'   age = c("Youth", "Adult", "Senior"),
#'   detection = c("Active", "Passive", "Active"),
#'   outcome = c("Asymptomatic", "Death", "Symptomatic")
#' )
#' population <- data.frame(
#'   age = c("Youth", "Adult", "Senior"),
#'   amount = rep(987L, 3L)
#' )
#' model <- SeverityEstimateModel(line_list, population) |>
#'   active_prior(mean = 0.9, sd = 0.08) |>
#'   passive_asymptomatic_prior(alpha = 1.0, beta = 1.0) |>
#'   passive_symptomatic_prior(mean = 0.1, var = 0.0064)
#' model
#'
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_list
#' @importFrom methods is
#' @importFrom methods slot<-
#' @export
#' @rdname prior
prior <- function(model, parameter, ...) {
  # Check the prior parameter name
  checkmate::assert_choice(
    parameter,
    c("active", "passive_asymptomatic", "passive_symptomatic")
  )
  parameter_prior <- paste0(parameter, "_prior")
  # Check the parameterization given
  args <- list(...)
  checkmate::assert_list(
    args,
    types = "numeric",
    any.missing = FALSE,
    len = 2L,
    names = "strict"
  )
  storage.mode(args) <- "double"
  # Check the model
  check_model(model, attribute = parameter_prior)
  # Assign parameterization to the model
  slot(model, parameter_prior) <- beta_parameterization(args)
  model
}

#' @rdname prior
#' @export
active_prior <- function(model, ...) {
  prior(model, "active", ...)
}

#' @rdname prior
#' @export
passive_asymptomatic_prior <- function(model, ...) {
  prior(model, "passive_asymptomatic", ...)
}

#' @rdname prior
#' @export
passive_symptomatic_prior <- function(model, ...) {
  prior(model, "passive_symptomatic", ...)
}
