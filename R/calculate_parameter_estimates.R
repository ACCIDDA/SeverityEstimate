#' @title
#' Calculate Key Parameter Estimates
#'
#' @description
#' Calculate a summary `data.frame` describing the key parameters fitted
#' including:
#'
#' * The mildly/asymptomatic passive detection rate,
#' * The severe symptoms passive detection rate, and
#' * The active detection rate.
#'
#' @param x A object to calculate fatality ratio statistics from, typically a
#' \linkS4class{SeverityEstimateFit} S4 object.
#' @param ... Further arguments passed to other methods.
#' @param mean_estimate A single logical indicating if the mean estimate for the
#' parameters should be included in the 'mean_estimate' column of the returned
#' `data.frame`.
#' @param median_estimate A single logical indicating if the median estimate for
#' the parameters should be included in the 'median_estimate column of the
#' returned `data.frame`.
#' @param alpha A numeric of significance levels to return the parameters
#' confidence intervals for. The columns will be in
#' '\{lower\verb{/}upper\}\verb{_}\{alpha\}'
#' format (i.e. 'lower\verb{_}05' and 'upper\verb{_}05' for `alpha=0.05`).
#' @param include_description A single logical indicating if descriptions of the
#' key parameters should be included in the return value. Setting this to `TRUE`
#' is handy for ad-hoc work, but setting to `FALSE` can be better for production
#' code. The description will be given in the 'parameter_description' column of
#' the returned `data.frame`.
#'
#' @return
#' `calculate_parameter_estimates.SeverityEstimateFit` returns a `data.frame`
#' with the column 'parameter'. Other columns are determined by other
#' parameters, see above for details.
#'
#' `calculate_fatality_ratio.default` signals an error.
#'
#' @export
calculate_parameter_estimates <- function(x, ...) {
  UseMethod("calculate_parameter_estimates")
}


#' @rdname calculate_parameter_estimates
#' @importFrom rstan extract
#' @export
calculate_parameter_estimates.SeverityEstimateFit <- function(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  include_description = TRUE,
  ...
) {
  calculate_parameter_estimates(
    x = rstan::extract(x@model_fit),
    mean_estimate = mean_estimate,
    median_estimate = median_estimate,
    alpha = alpha,
    include_description = include_description
  )
}


#' @inheritParams calculate_parameter_estimates
#' @importFrom stats quantile
#' @keywords internal
#' @export
calculate_parameter_estimates.list <- function(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  include_description = TRUE,
  ...
) {
  # Input validation
  stopifnot(
    all(
      c(
        "active_detection",
        "passive_asymptomatic_detection",
        "passive_symptomatic_detection"
      ) %in%
        names(x)
    )
  )
  stopifnot(isTRUE(mean_estimate) || isFALSE(mean_estimate))
  stopifnot(isTRUE(median_estimate) || isFALSE(median_estimate))
  stopifnot(isTRUE(include_description) || isFALSE(include_description))
  if (!any(mean_estimate, median_estimate, length(alpha) > 0L)) {
    stop(
      "At least one of following must be true: `mean_estimate` is `TRUE`, ",
      "`median_estimate` is `TRUE`, or `alpha` is non-empty."
    )
  }
  if (length(alpha) > 0L) {
    confidence_bounds <- process_significance_levels(alpha)
    conf_probs <- as.numeric(confidence_bounds)
    conf_labels <- paste0(
      rep_len(rownames(confidence_bounds), length(conf_probs)),
      "_",
      rep(colnames(confidence_bounds), each = 2L)
    )
  }

  # Extract and process values
  params_matrix <- cbind(
    x$active_detection,
    x$passive_asymptomatic_detection,
    x$passive_symptomatic_detection
  )
  idx <- if (median_estimate) 2L else 1L
  param_summary <- do.call(
    rbind,
    apply(params_matrix, 2L, function(x) {
      lst <- list()
      # Mean estimate
      if (mean_estimate) {
        lst[["mean_estimate"]] <- mean(x)
      }
      # Median estimate and CIs
      if (median_estimate || length(alpha) > 0L) {
        q <- stats::quantile(
          x,
          probs = c(
            if (median_estimate) 0.5 else numeric(),
            if (length(alpha) > 0L) conf_probs else numeric()
          ),
          names = FALSE
        )
        if (median_estimate) {
          lst[["median_estimate"]] <- q[1L]
        }
        if (length(alpha) > 0L) {
          lst[conf_labels] <- q[idx:length(q)]
        }
      }
      # Done
      as.data.frame(lst)
    })
  )

  # Format return
  old_names <- names(param_summary)
  param_summary$parameter <- c(
    "active_detection",
    "passive_asymptomatic_detection",
    "passive_symptomatic_detection"
  )
  if (include_description) {
    param_summary$parameter_description <- c(
      "active detection rate",
      "mildly/asymptomatic passive detection rate",
      "severe symptoms passive detection rate"
    )
    new_names <- c("parameter", "parameter_description", old_names)
  } else {
    new_names <- c("parameter", old_names)
  }
  param_summary <- param_summary[, new_names]

  # Done
  param_summary
}


#' @rdname calculate_parameter_estimates
#' @export
calculate_parameter_estimates.default <- function(x, ...) {
  stop(
    "Unable to find a suitable `calculate_parameter_estimates` method for `x` ",
    "with classes: ",
    toString(class(x)),
    "."
  )
}
