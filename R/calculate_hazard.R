#' @title
#' Calculate Hazard Statistics
#'
#' @description
#' Calculate a `data.frame` of posterior infection-hazard estimates broken down
#' by time period and strata.
#'
#' @param x An object to calculate hazard statistics from, typically a
#' \linkS4class{SeverityEstimateFit} S4 object.
#' @param ... Further arguments passed to other methods.
#' @param time_period A `data.frame` describing the time-period dimension of the
#' underlying hazard.
#' @param strata A `data.frame` describing the strata dimension of the
#' underlying hazard.
#' @param population Optional numeric vector describing the population for each
#' strata row. If provided, strata with zero population are excluded from the
#' returned hazard summary.
#' @param mean_estimate A single logical indicating if the mean estimate for the
#' hazard should be included in the `mean_estimate` column of the returned
#' `data.frame`.
#' @param median_estimate A single logical indicating if the median estimate for
#' the hazard should be included in the `median_estimate` column of the
#' returned `data.frame`.
#' @param alpha A numeric of significance levels to return the hazard
#' confidence intervals for. The columns will be in
#' `\{lower/upper\}_\{alpha\}` format
#' (i.e. `lower_05` and `upper_05` for `alpha=0.05`).
#'
#' @return
#' `calculate_hazard.SeverityEstimateFit` returns a `data.frame` describing
#' posterior hazard estimates by time period and strata.
#'
#' `calculate_hazard.default` signals an error.
#'
#' @examples
#' logit_hzd <- array(
#'   qlogis(seq(0.01, 0.08, length.out = 8L)),
#'   dim = c(2L, 2L, 2L)
#' )
#' calculate_hazard(
#'   list(logit_hzd = logit_hzd),
#'   time_period = data.frame(week = 1L:2L),
#'   strata = data.frame(age = c("Adult", "Senior")),
#'   alpha = numeric()
#' )
#'
#' @export
calculate_hazard <- function(x, ...) {
  UseMethod("calculate_hazard")
}


#' @rdname calculate_hazard
#' @export
calculate_hazard.SeverityEstimateFit <- function(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  ...
) {
  hazard <- calculate_hazard(
    x = backend_extract(x@model_fit, "logit_hzd"),
    time_period = x@time_period,
    strata = x@strata,
    population = x@population,
    mean_estimate = mean_estimate,
    median_estimate = median_estimate,
    alpha = alpha,
    ...
  )
  attr(hazard, "time_period_reference") <- x@time_period
  attr(hazard, "strata_reference") <- positive_population_strata(
    x@strata,
    x@population
  )
  hazard
}


#' @rdname calculate_hazard
#' @importFrom stats quantile
#' @export
calculate_hazard.list <- function(
  x,
  time_period,
  strata,
  population = NULL,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  alpha = 0.05,
  ...
) {
  # Input validation
  checkmate::assert_list(x, names = "named")
  checkmate::assert_names(names(x), must.include = "logit_hzd")
  checkmate::assert_data_frame(time_period)
  checkmate::assert_names(names(time_period), type = "unique")
  checkmate::assert_data_frame(strata)
  checkmate::assert_names(names(strata), type = "unique")
  assert_bool(mean_estimate)
  assert_bool(median_estimate)
  if (!any(mean_estimate, median_estimate, length(alpha) > 0L)) {
    stop(
      "At least one of following must be true: `mean_estimate` is `TRUE`, ",
      "`median_estimate` is `TRUE`, or `alpha` is non-empty."
    )
  }

  logit_hzd <- x$logit_hzd
  checkmate::assert_array(
    logit_hzd,
    mode = "numeric",
    any.missing = FALSE,
    min.d = 3L,
    max.d = 3L
  )
  hazard_dim <- dim(logit_hzd)
  if (hazard_dim[2L] != nrow(time_period)) {
    stop(
      "The rows of `time_period` must match the second dimension of ",
      "`x$logit_hzd`."
    )
  }
  if (hazard_dim[3L] != nrow(strata)) {
    stop(
      "The rows of `strata` must match the third dimension of ",
      "`x$logit_hzd`."
    )
  }
  if (!is.null(population)) {
    checkmate::assert_numeric(
      population,
      any.missing = FALSE,
      lower = 0.0,
      len = nrow(strata)
    )
    population <- as.numeric(population)
    positive_population <- population > 0.0
    if (!any(positive_population)) {
      stop(
        "At least one strata group must have a positive population.",
        call. = FALSE
      )
    }
    logit_hzd <- logit_hzd[,, positive_population, drop = FALSE]
    strata <- strata[positive_population, , drop = FALSE]
    hazard_dim <- dim(logit_hzd)
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

  # Reorder the draws into a cell-by-sample matrix with time-major ordering.
  hazard_draws <- inv_logit(logit_hzd) |>
    aperm(c(3L, 2L, 1L))
  dim(hazard_draws) <- c(nrow(time_period) * nrow(strata), hazard_dim[1L])

  time_idx <- rep(seq_len(nrow(time_period)), each = nrow(strata))
  strata_idx <- rep(seq_len(nrow(strata)), times = nrow(time_period))
  hazard <- cbind(
    time_period[time_idx, , drop = FALSE],
    strata[strata_idx, , drop = FALSE]
  )
  rownames(hazard) <- NULL

  if (mean_estimate) {
    hazard$mean_estimate <- rowMeans(hazard_draws)
  }
  if (median_estimate || length(alpha) > 0L) {
    probs <- c(
      if (median_estimate) 0.5 else numeric(),
      if (length(alpha) > 0L) conf_probs else numeric()
    )
    quantile_names <- c(
      if (median_estimate) "median_estimate" else character(),
      if (length(alpha) > 0L) conf_labels else character()
    )
    hazard_quantiles <- apply(
      hazard_draws,
      1L,
      stats::quantile,
      probs = probs,
      names = FALSE
    )
    if (length(probs) == 1L) {
      hazard_quantiles <- matrix(hazard_quantiles, ncol = 1L)
    } else {
      hazard_quantiles <- t(hazard_quantiles)
    }
    hazard_quantiles <- as.data.frame(hazard_quantiles)
    names(hazard_quantiles) <- quantile_names
    hazard <- cbind(hazard, hazard_quantiles)
  }

  hazard
}


#' @rdname calculate_hazard
#' @export
calculate_hazard.default <- function(x, ...) {
  stop(
    "Unable to find a suitable `calculate_hazard` method for `x` ",
    "with classes: ",
    toString(class(x)),
    "."
  )
}

positive_population_strata <- function(strata, population) {
  population <- as.numeric(population)
  if (length(population) != nrow(strata)) {
    return(strata)
  }
  strata[population > 0.0, , drop = FALSE]
}
