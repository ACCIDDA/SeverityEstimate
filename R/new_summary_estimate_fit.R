#' @name summary_estimate_fit_helpers
#' @title
#' Internal Helpers For `SummaryEstimateFit`
#'
#' @description
#' Internal helpers used to construct and format `SummaryEstimateFit` objects.
#'
#' @param detection_rates A `data.frame` of detection-rate estimates.
#' @param severity_estimates A `data.frame` of IFR/SIR estimates, optionally
#' including strata columns.
#'
#' @return
#' `new_summary_estimate_fit` returns an object of class `SummaryEstimateFit`
#' with elements `detection_rates` and `severity_estimates`.
#'
#' `format_summary_detection_rates` returns a `data.frame` of mean detection
#' rate estimates formatted for printing.
#'
#' `format_summary_severity_estimates` returns a `data.frame` of mean IFR/SIR
#' estimates with strata columns first and renamed estimate columns.
#'
#' @noRd
NULL


#' @noRd
new_summary_estimate_fit <- function(detection_rates, severity_estimates) {
  structure(
    list(
      detection_rates = detection_rates,
      severity_estimates = severity_estimates
    ),
    class = "SummaryEstimateFit"
  )
}


#' @noRd
format_summary_detection_rates <- function(detection_rates) {
  detection_order <- c(
    "passive_asymptomatic_detection",
    "passive_symptomatic_detection",
    "active_detection"
  )
  detection_labels <- c(
    passive_asymptomatic_detection = "passive_asymptomatic",
    passive_symptomatic_detection = "passive_symptomatic",
    active_detection = "active"
  )
  detection_idx <- match(detection_order, detection_rates$parameter)
  data.frame(
    Estimate = detection_rates$mean_estimate[detection_idx],
    row.names = unname(detection_labels[detection_order]),
    check.names = FALSE
  )
}


#' @noRd
format_summary_severity_estimates <- function(severity_estimates) {
  strata_reference <- attr(
    severity_estimates,
    "strata_reference",
    exact = TRUE
  )
  if (is.data.frame(strata_reference)) {
    severity_estimates <- reorder_by_strata(
      severity_estimates,
      strata_reference
    )
  }
  estimate_cols <- c("ifr_mean_estimate", "sir_mean_estimate")
  strata_cols <- setdiff(names(severity_estimates), estimate_cols)
  severity_estimates <- severity_estimates[,
    c(strata_cols, estimate_cols),
    drop = FALSE
  ]
  names(severity_estimates)[
    names(severity_estimates) == "ifr_mean_estimate"
  ] <- "IFR Estimate"
  names(severity_estimates)[
    names(severity_estimates) == "sir_mean_estimate"
  ] <- "SIR Estimate"
  severity_estimates
}
