#' @title
#' Severity Estimate Fit Class
#'
#' @description
#' This class contains the output from a severity estimate model fitting.
#'
#' @section Functions and methods:
#' - `summary(object)` summarises a fitted severity estimate model by reporting
#'   mean detection rate estimates and mean IFR/SIR estimates by strata.
#' - `print.SeverityEstimateFit(x)` prints a \linkS4class{SeverityEstimateFit}
#'   object in a structured format. Currently this prints the `model_fit` slot
#'   using the `print` method for a `stanfit` object.
#' - `print.SummaryEstimateFit(x, digits)` prints a `SummaryEstimateFit` object
#'   in a structured format.
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
#' @return
#' A function-dependent value:
#' - `summary.SeverityEstimateFit()` returns a `SummaryEstimateFit` with
#'   elements `detection_rates` and `severity_estimates`.
#' - `print.SeverityEstimateFit()` and `print.SummaryEstimateFit()` invisibly
#'   return their input object.
#'
#' @examples
#' \dontrun{
#' model <- default_model(line_list, population)
#' fitted_model <- fit(model, chains = 1L, iter = 100L)
#' summary(fitted_model)
#' }
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


#' @param x An object of class \linkS4class{SeverityEstimateFit} or
#' `SummaryEstimateFit`.
#' @param ... For `summary.SeverityEstimateFit()`, unused. For
#' `print.SeverityEstimateFit()`, further arguments passed to the `print`
#' method for a `stanfit` object. For `print.SummaryEstimateFit()`, further
#' arguments passed to [print.data.frame()].
#'
#' @export
#' @rdname SeverityEstimateFit
print.SeverityEstimateFit <- function(x, ...) {
  # For now just fallback to stan's print method
  print(x@model_fit, ...)
}


#' @param object An object of class \linkS4class{SeverityEstimateFit}.
#'
#' @export
#' @rdname SeverityEstimateFit
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


#' @param digits The number of significant digits to print.
#'
#' @export
#' @rdname SeverityEstimateFit
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
