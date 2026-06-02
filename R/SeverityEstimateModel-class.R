#' @title
#' Severity Estimate Model
#'
#' @description
#' A representation of a severity estimate model and its metadata.
#'
#' @section Functions and methods:
#' - `SeverityEstimateModel(line_list, population)` creates a
#'   \linkS4class{SeverityEstimateModel} object.
#' - `summary(object)` summarises a user-defined severity estimate model by
#'   reporting input data dimensions, detection probability priors, timestep
#'   bounds, mapped detection and outcome counts, and strata specifications.
#' - `print.SeverityEstimateModel(x)` prints a compact summary of a
#'   \linkS4class{SeverityEstimateModel} object.
#' - `show(object)` shows a compact summary of a
#'   \linkS4class{SeverityEstimateModel} object.
#' - `print.SummaryEstimateModel(x, digits)` prints a `SummaryEstimateModel`
#'   object in a structured format.
#'
#' @slot line_list A line list of cases to model the severity of.
#' @slot population A dataset containing information on the population broken
#' down by stratification.
#' @slot strata A list of model stratification specifications.
#' @slot timesteps A list specifying the timestep column of the linelist.
#' @slot detection A list specifying the detection type mapping.
#' @slot outcome A list specifying the outcome severity mapping.
#' @slot active_prior Parameters for the beta distribution prior for the active
#' detection rate.
#' @slot passive_asymptomatic_prior Parameters for the beta distribution prior
#' for the passive asymptomatic detection rate.
#' @slot passive_symptomatic_prior Parameters for the beta distribution prior
#' for the passive symptomatic detection rate.
#'
#' @return
#' A function-dependent value:
#' - `SeverityEstimateModel()` returns a \linkS4class{SeverityEstimateModel}
#'   object.
#' - `summary.SeverityEstimateModel()` returns a `SummaryEstimateModel`.
#' - `print.SeverityEstimateModel()`, `show()`, and
#'   `print.SummaryEstimateModel()` invisibly return their input object.
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
#' model <- SeverityEstimateModel(line_list, population)
#' summary(model)
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
    "timesteps" = "list",
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
    "timesteps" = list(),
    "detection" = list(),
    "outcome" = list(),
    "active_prior" = numeric(),
    "passive_asymptomatic_prior" = numeric(),
    "passive_symptomatic_prior" = numeric()
  )
)


#' @param line_list A line list of cases to model the severity of.
#' @param population A dataset containing information on the population broken
#' down by stratification. Can also be a single integer in the case that the
#' model is not stratified.
#'
#' @importFrom checkmate test_integerish
#' @importFrom methods new
#' @export
#' @rdname SeverityEstimateModel
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


#' @param object An object of class \linkS4class{SeverityEstimateModel}.
#' @param ... For `summary.SeverityEstimateModel()` and
#' `print.SummaryEstimateModel()`, unused. For `print.SeverityEstimateModel()`,
#' further arguments passed to [print.SummaryEstimateModel()].
#'
#' @export
#' @rdname SeverityEstimateModel
summary.SeverityEstimateModel <- function(object, ...) {
  new_summary_estimate_model(
    data = format_summary_model_data(object),
    priors = format_summary_model_priors(object),
    timesteps = format_summary_model_timesteps(object),
    detection = format_summary_model_detection(object),
    outcome = format_summary_model_outcome(object),
    strata = format_summary_model_strata(object)
  )
}


#' @param x An object of class \linkS4class{SeverityEstimateModel} or
#' `SummaryEstimateModel`.
#'
#' @export
#' @rdname SeverityEstimateModel
print.SeverityEstimateModel <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}


#' @importFrom methods setMethod
#' @importFrom methods signature
#' @export
#' @rdname SeverityEstimateModel
methods::setMethod(
  "show",
  methods::signature(object = "SeverityEstimateModel"),
  function(object) {
    print(summary(object))
    invisible(object)
  }
)


#' @param digits The number of significant digits to print for prior
#' parameters.
#'
#' @export
#' @rdname SeverityEstimateModel
print.SummaryEstimateModel <- function(
  x,
  digits = max(3L, getOption("digits") - 3L),
  ...
) {
  cat("Severity Estimate Model:\n")

  cat("\nData:\n")
  print(x$data, row.names = FALSE, ...)

  cat("\nDetection Probability Priors:\n")
  for (idx in seq_len(nrow(x$priors))) {
    prior <- x$priors[idx, , drop = FALSE]
    default_text <- if (prior$default) " (default)" else ""
    cat(
      "  ",
      prior$parameter,
      " prior: beta(",
      format_summary_model_number(prior$alpha, digits = digits),
      ", ",
      format_summary_model_number(prior$beta, digits = digits),
      ")",
      default_text,
      "\n",
      sep = ""
    )
  }

  cat("\nTimesteps:\n")
  if (nrow(x$timesteps)) {
    cat(
      "  ",
      x$timesteps$column,
      ": ",
      x$timesteps$start,
      " to ",
      x$timesteps$end,
      " (",
      x$timesteps$timesteps,
      " timesteps)\n",
      sep = ""
    )
  } else {
    cat("  not set\n")
  }

  cat("\nDetection:\n")
  print_summary_model_map(x$detection)

  cat("\nOutcome:\n")
  print_summary_model_map(x$outcome)

  cat("\nStrata:\n")
  if (nrow(x$strata)) {
    for (idx in seq_len(nrow(x$strata))) {
      strata <- x$strata[idx, , drop = FALSE]
      cat(
        "  ",
        strata$column,
        ": ",
        strata$n_levels,
        " levels, df = ",
        strata$degrees_of_freedom,
        " (",
        strata$levels,
        ")\n",
        sep = ""
      )
    }
  } else {
    cat("  none\n")
  }

  invisible(x)
}
