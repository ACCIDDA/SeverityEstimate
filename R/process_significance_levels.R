#' @title
#' Convert Significance Levels Into Confidence Interval Bounds
#'
#' @description
#' Calculate the lower and upper bounds of a confidence interval from a
#' significance level.
#'
#' @param alpha A numeric of significance levels to consider. Only the first two
#' decimals are considered and the values in this numeric must be unique.
#'
#' @return
#' A numeric matrix of two rows, with dimnames "lower" and "upper" and
#' `length(alpha)` columns, with dimnames of `alpha` formatted as a two digit
#' number like '05' or '10' for `0.05` or `0.1`.
#'
#' @keywords internal
process_significance_levels <- function(alpha) {
  # Input validation/processing
  stopifnot(
    is.numeric(alpha),
    !anyNA(alpha),
    all(0.0 < alpha & alpha < 1.0)
  )
  alpha <- round(alpha, digits = 2L)
  if (length(alpha) != length(unique(alpha))) {
    stop(
      "The entries in `alpha` are not unique when rounded to 2 digits, ",
      "was given: ", toString(alpha), "."
    )
  }

  # Calculate confidence interval bounds
  confidence_bounds <- sapply(alpha, \(a) c(0.5 * a, 1.0 - (0.5 * a)))
  dimnames(confidence_bounds) <- list(
    "bound" = c("lower", "upper"),
    "significance" = formatC(
      100.0 * alpha,
      width = 2L,
      format = "d",
      flag = "0"
    )
  )

  # Done
  confidence_bounds
}
