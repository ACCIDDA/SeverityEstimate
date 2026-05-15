#' @title
#' Severity Estimate Fit Class
#'
#' @description
#' This class contains the output from a severity estimate model fitting.
#'
#' @slot model_fit A stanfit object returned from fitting a severity estimate
#' model.
#' @slot population The population data used in model fitting in array form with
#' dimensions corresponding to 'time_period' and 'strata'.
#' @slot incidence The line list data used in model fitting in array form
#' counting incidence with dimensions corresponding to 'time_period', 'strata',
#' 'surveillance', and 'outcome'.
#' @slot time_period A data.frame with the variables describing the
#' 'time_period' dimensions of `population` and `incidence`.
#' @slot strata A data.frame with the variables describing the 'strata'
#' dimensions of `population` and `incidence`.
#' @slot surveillance A data.frame with the variables describing the
#' 'surveillance' dimension of `incidence`.
#' @slot outcome A data.frame with the variables describing the 'outcome'
#' dimension of `incidence`.
#'
#' @importFrom methods setClass
#' @export
#' @rdname SeverityEstimateFit
setClass(
  Class = "SeverityEstimateFit",
  slots = c(
    "model_fit" = "stanfit",
    "population" = "array",
    "incidence" = "array",
    "time_period" = "data.frame",
    "strata" = "data.frame",
    "surveillance" = "data.frame",
    "outcome" = "data.frame"
  )
)


#' @title
#' Print Method for `SeverityEstimateFit` Objects
#'
#' @description
#' Prints a `SeverityEstimateFit` object in a structured format. Currently just
#' prints the `model_fit` slot using the `print` method for a `stanfit` object.
#'
#' @param x An object of class `SeverityEstimateFit`.
#' @param ... Further arguments passed to the `print` method for a `stanfit`
#' object.
#'
#' @return
#' `x` invisibly.
#'
#' @export
print.SeverityEstimateFit <- function(x, ...) {
  # For now just fallback to stan's print method
  print(x@model_fit, ...)
}


#' @title
#' Summary Method for `SeverityEstimateFit` Objects
#'
#' @description
#' Summarises a fitted severity estimate model by reporting mean detection rate
#' estimates and mean IFR/SIR estimates by strata.
#'
#' @param object An object of class `SeverityEstimateFit`.
#' @param ... Unused.
#'
#' @return
#' `summary.SeverityEstimateFit` returns an object of class
#' `SummaryEstimateFit` with elements `detection_rates` and
#' `severity_estimates`.
#'
#' @export
summary.SeverityEstimateFit <- function(object, ...) {
  detection_rates <- calculate_parameter_estimates(
    object,
    mean_estimate = TRUE,
    median_estimate = FALSE,
    alpha = numeric(),
    include_description = FALSE
  )
  severity_estimates <- calculate_fatality_ratio(
    object,
    mean_estimate = TRUE,
    median_estimate = FALSE,
    naive_estimate = FALSE,
    alpha = numeric()
  )
  new_summary_estimate_fit(
    detection_rates = format_summary_detection_rates(detection_rates),
    severity_estimates = format_summary_severity_estimates(severity_estimates)
  )
}


#' @title
#' Print Method for `SummaryEstimateFit` Objects
#'
#' @description
#' Prints a `SummaryEstimateFit` object in a structured format.
#'
#' @param x An object of class `SummaryEstimateFit`.
#' @param digits The number of significant digits to print.
#' @param ... Further arguments passed to [print.data.frame()].
#'
#' @return
#' `x` invisibly.
#'
#' @export
print.SummaryEstimateFit <- function(
  x,
  digits = max(3L, getOption("digits") - 3L),
  ...
) {
  cat("Detection Rates:\n")
  print(
    x$detection_rates,
    digits = digits,
    quote = FALSE,
    right = TRUE,
    ...
  )
  cat("\nSeverity Estimates:\n")
  print(
    x$severity_estimates,
    digits = digits,
    quote = FALSE,
    right = TRUE,
    row.names = FALSE,
    ...
  )
  invisible(x)
}
