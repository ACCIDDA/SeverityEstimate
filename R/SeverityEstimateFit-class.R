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
#' @slot incidents The line list data used in model fitting in array form
#' counting incidents with dimensions corresponding to 'time_period', 'strata',
#' 'surveillance', and 'outcome'.
#' @slot time_period A data.frame with the variables describing the
#' 'time_period' dimensions of `population` and `incidents`.
#' @slot strata A data.frame with the variables describing the 'strata'
#' dimensions of `population` and `incidents`.
#' @slot surveillance A data.frame with the variables describing the
#' 'surveillance' dimension of `incidents`.
#' @slot outcome A data.frame with the variables describing the 'outcome'
#' dimension of `incidents`.
#'
#' @importFrom methods setClass
#' @export
#' @rdname SeverityEstimateFit
setClass(
  Class = "SeverityEstimateFit",
  slots = c(
    "model_fit" = "stanfit",
    "population" = "array",
    "incidents" = "array",
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
