#' @title
#' Severity Estimate Model
#'
#' @description
#' A representation of a severity estimate model and its metadata.
#'
#' @slot line_list A line list of cases to model the severity of.
#' @slot population A dataset containing information on the population broken
#' down by strataification.
#' @slot strata A list of model stratification specifications.
#' @slot time A list specifying the time column of the linelist.
#' @slot detection A list specifying the detection type mapping.
#' @slot outcome A list specifying the outcome severity mapping.
#' @slot active_prior Parameters for the beta distribution prior for the active
#' detection rate.
#' @slot passive_asymptomatic_prior Parameters for the beta distribution prior
#' for the passive asymptomatic detection rate.
#' @slot passive_symptomatic_prior Parameters for the beta distribution prior
#' for the passive symptomatic detection rate.
#'
#' @importFrom methods setClass
#' @export
#' @rdname SeverityEstimateModel
setClass(
  Class = "SeverityEstimateModel",
  slots = c(
    "line_list" = "data.frame",
    "population" = "data.frame",
    "strata" = "list",
    "time" = "list",
    "detection" = "list",
    "outcome" = "list",
    "active_prior" = "numeric",
    "passive_asymptomatic_prior" = "numeric",
    "passive_symptomatic_prior" = "numeric"
  ),
  prototype = list(
    "line_list" = data.frame(),
    "population" = data.frame(),
    "strata" = list(),
    "time" = list(),
    "detection" = list(),
    "outcome" = list(),
    "active_prior" = numeric(),
    "passive_asymptomatic_prior" = numeric(),
    "passive_symptomatic_prior" = numeric()
  )
)


#' @title
#' Create A Severity Model Instance
#'
#' @param line_list A line list of cases to model the severity of.
#' @param population A dataset containing information on the population broken
#' down by strataification. Can also be a single integer in the case that the
#' model is not stratafied.
#'
#' @return
#' A \linkS4class{SeverityEstimateModel} S4 object instance representing a model
#' and its associated metadata.
#'
#' @importFrom checkmate test_integerish
#' @importFrom methods new
#' @export
SeverityEstimateModel <- function(line_list, population) {
  line_list <- is_data_frame(line_list)
  if (
    checkmate::test_integerish(
      population,
      len = 1L,
      lower = 0L,
      any.missing = FALSE
    )
  ) {
    population <- data.frame(value = population)
  }
  population <- is_data_frame(population)
  methods::new(
    "SeverityEstimateModel",
    line_list = line_list,
    population = population
  )
}


#' @title
#' Print Method for `SeverityEstimateModel` Objects
#'
#' @description
#' Prints a \linkS4class{SeverityEstimateModel} object in a structured format.
#' Currently just a thin wrapper around [utils::str()].
#'
#' @param x An object of class \linkS4class{SeverityEstimateModel}.
#' @param ... Further arguments passed to the [utils::str()] function.
#'
#' @return
#' `x` invisibly.
#'
#' @importFrom utils str
#' @export
print.SeverityEstimateModel <- function(x, ...) {
  # For now just fallback to stan's print method
  utils::str(x, ...)
  invisible(x)
}
