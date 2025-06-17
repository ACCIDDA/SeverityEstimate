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


#' @title
#' Resolve Beta Distribution Parameterization
#'
#' @param params A named numeric of length two that represent the
#' parameterization of a beta distribution.
#'
#' @return
#' A numeric of length two with names 'alpha' and 'beta' corresponding to the
#' `params` converted to alpha/beta parameterization.
#'
#' @importFrom checkmate assert_number
#' @importFrom checkmate assert_numeric
#' @keywords internal
beta_parameterization <- function(params) {
  checkmate::assert_numeric(
    params,
    finite = TRUE,
    any.missing = FALSE,
    len = 2L,
    names = "strict"
  )
  params_names <- names(params)
  if (setequal(params_names, c("alpha", "beta"))) {
    checkmate::assert_numeric(params, lower = .Machine$double.eps)
    return(params)
  }
  if (
    setequal(params_names, c("mean", "var")) ||
      setequal(params_names, c("mean", "sd"))
  ) {
    dispersion <- ifelse("var" %in% params_names, "var", "sd")
    checkmate::assert_number(
      params["mean"],
      lower = .Machine$double.eps,
      upper = 1.0 - .Machine$double.eps
    )
    checkmate::assert_number(
      params[dispersion],
      lower = .Machine$double.eps,
      .var.name = paste0('params["', dispersion, '"]')
    )
    if (dispersion == "sd") {
      params["var"] <- params["sd"]**2.0
    }
    nu <- ((params["mean"] * (1.0 - params["mean"])) / params["var"]) - 1.0
    return(
      c(
        "alpha" = unname(params["mean"] * nu),
        "beta" = unname((1.0 - params["mean"]) * nu)
      )
    )
  }
  stop(
    "The given parameterization ",
    toString(params_names),
    " is not ",
    "recognized. Must be one of 'alpha'/'beta', 'mean'/'var', 'mean'/'sd'."
  )
}
