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
#' Summary Method for `SeverityEstimateModel` Objects
#'
#' @description
#' Summarises a user-defined severity estimate model by reporting input data
#' dimensions, detection probability priors, timestep bounds, mapped detection
#' and outcome counts, and strata specifications.
#'
#' @param object An object of class \linkS4class{SeverityEstimateModel}.
#' @param ... Unused.
#'
#' @return
#' `summary.SeverityEstimateModel` returns an object of class
#' `SummaryEstimateModel`.
#'
#' @export
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


#' @title
#' Print Method for `SeverityEstimateModel` Objects
#'
#' @description
#' Prints a compact summary of a \linkS4class{SeverityEstimateModel} object.
#'
#' @param x An object of class \linkS4class{SeverityEstimateModel}.
#' @param object An object of class \linkS4class{SeverityEstimateModel}.
#' @param ... Further arguments passed to [print.SummaryEstimateModel()].
#'
#' @return
#' `x` invisibly.
#'
#' @export
print.SeverityEstimateModel <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}


#' @rdname print.SeverityEstimateModel
#' @return
#' `object` invisibly.
#'
#' @importFrom methods setMethod
#' @importFrom methods signature
#' @export
methods::setMethod(
  "show",
  methods::signature(object = "SeverityEstimateModel"),
  function(object) {
    print(summary(object))
    invisible(object)
  }
)


#' @title
#' Print Method for `SummaryEstimateModel` Objects
#'
#' @description
#' Prints a `SummaryEstimateModel` object in a structured format.
#'
#' @param x An object of class `SummaryEstimateModel`.
#' @param digits The number of significant digits to print for prior
#' parameters.
#' @param ... Unused.
#'
#' @return
#' `x` invisibly.
#'
#' @export
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
