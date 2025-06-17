#' @title
#' Calculate Fatality Ratio Statistics
#'
#' @description
#' Calculate a `data.frame` of fatality ratios broken down by strata, as well
#' as other optional model metrics.
#'
#' @param x A object to calculate fatality ratio statistics from, typically a
#' \linkS4class{SeverityEstimateFit} S4 object.
#' @param ... Further arguments passed to other methods.
#' @param mean_estimate A single logical indicating if the mean estimate for the
#' ratios should be included in the '\verb{*_}mean\verb{_}estimate' columns of
#' the returned `data.frame`.
#' @param median_estimate A single logical indicating if the median estimate for
#' the ratios should be included in the '\verb{*_}median\verb{_}estimate column
#' of the returned `data.frame`.
#' @param naive_estimate A single logical indicating if the naive estimate for
#' fatality ratio should be included in the 'naive\verb{_}estimate' column.
#' @param alpha A numeric of significance levels to return the parameters
#' confidence intervals for. The columns will be in
#' '\verb{*_}\{lower\verb{/}upper\}\verb{_}\{alpha\}' format
#' (i.e. '\verb{*_}lower\verb{_}05' and '\verb{*_}upper\verb{_}05'
#' for `alpha=0.05`).
#'
#' @return
#' `calculate_fatality_ratio.SeverityEstimateFit` returns a `data.frame`
#' describing fatality ratios by strata or if now strata were provided to when
#' fitting a single row `data.frame`.
#'
#' `calculate_fatality_ratio.default` signals an error.
#'
#' @export
calculate_fatality_ratio <- function(x, ...) {
  UseMethod("calculate_fatality_ratio")
}


#' @rdname calculate_fatality_ratio
#' @importFrom rstan extract
#' @export
calculate_fatality_ratio.SeverityEstimateFit <- function(
  x,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  naive_estimate = FALSE,
  alpha = 0.05,
  ...
) {
  calculate_fatality_ratio(
    x = rstan::extract(x@model_fit, c("C", "mortality", "xi")),
    strata = x@strata,
    mean_estimate = mean_estimate,
    median_estimate = median_estimate,
    naive_estimate = naive_estimate,
    alpha = alpha,
    incidence = x@incidence,
    outcome = x@outcome,
    ...
  )
}


#' @inheritParams calculate_fatality_ratio
#' @importFrom stats quantile
#' @keywords internal
#' @export
calculate_fatality_ratio.list <- function(
  x,
  strata,
  mean_estimate = TRUE,
  median_estimate = TRUE,
  naive_estimate = FALSE,
  alpha = 0.05,
  incidence = NULL,
  outcome = NULL,
  ...
) {
  # Input validation
  stopifnot(all(c("C", "mortality", "xi") %in% names(x)))
  stopifnot(isTRUE(mean_estimate) || isFALSE(mean_estimate))
  stopifnot(isTRUE(median_estimate) || isFALSE(median_estimate))
  if (length(alpha) > 0L) {
    confidence_bounds <- process_significance_levels(alpha)
    conf_probs <- as.numeric(confidence_bounds)
    conf_labels <- paste0(
      rep_len(rownames(confidence_bounds), length(conf_probs)),
      "_",
      rep(colnames(confidence_bounds), each = 2L)
    )
  }

  # Calculation
  fatality_ratios <- strata

  if (mean_estimate) {
    fatality_ratios$ifr_mean_estimate <- apply(x$mortality, 2L, mean)
    fatality_ratios$sir_mean_estimate <- apply(x$xi, 2L, mean)
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
    calc_quantiles_data_frame <- function(x, prefix) {
      x <- x |>
        apply(2L, stats::quantile, probs = probs, names = FALSE) |>
        {
          \(.) if (length(probs) > 1L) t(.) else .
        }() |>
        as.data.frame()
      names(x) <- paste0(prefix, "_", quantile_names)
      x
    }
    ifr_quantiles <- calc_quantiles_data_frame(x$mortality, "ifr")
    sir_quantiles <- calc_quantiles_data_frame(x$xi, "sir")
    fatality_ratios <- cbind(fatality_ratios, ifr_quantiles, sir_quantiles)
  }

  # Naive estimates branch
  if (naive_estimate) {
    reduced_incidence <- apply(incidence, c(2L, 4L), sum)
    total_incidence <- rowSums(reduced_incidence)
    reduced_incidence <- cbind(
      reduced_incidence,
      rep.int(0L, nrow(reduced_incidence))
    )
    outcome_ind <- match(
      c("Death", "Symptomatic"),
      outcome[, 1L],
      nomatch = ncol(reduced_incidence)
    )
    fatality_ratios$naive_ifr <- (reduced_incidence[, outcome_ind[1L]] /
      total_incidence)
    fatality_ratios$naive_sir <- (rowSums(reduced_incidence[, outcome_ind]) /
      total_incidence)
  }

  # Reorder the column names for pretty output
  old_colnames <- colnames(fatality_ratios)
  new_colnames <- c(
    colnames(strata),
    old_colnames[grepl("^ifr\\_.*", old_colnames)],
    old_colnames[grepl("^sir\\_.*", old_colnames)],
    old_colnames[grepl("^naive\\_.*", old_colnames)]
  )
  fatality_ratios <- fatality_ratios[, new_colnames]

  # Done
  fatality_ratios
}


#' @rdname calculate_fatality_ratio
#' @export
calculate_fatality_ratio.default <- function(x, ...) {
  stop(
    "Unable to find a suitable `calculate_fatality_ratio` method for `x` ",
    "with classes: ",
    toString(class(x)),
    "."
  )
}
