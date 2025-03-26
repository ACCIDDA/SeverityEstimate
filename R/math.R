#' @title
#' Logit And Inverse Logit
#'
#' @description
#' Math utilities for converting between a probability and real valued numbers.
#'
#' @param p A probability to convert to a real value.
#' @param x A real value to convert to probability.
#'
#' @details
#' These functions are named to mimic their stan equivalents.
#'
#' @keywords internal
logit <- function(p) {
  log(p / (1.0 - p))
}

#' @rdname logit
inv_logit <- function(x) {
  1.0 / (1.0 + exp(-x))
}
