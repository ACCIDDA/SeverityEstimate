#' @title
#' Estimate Model Parameters For A Severity Estimate Model
#'
#' @description
#' Estimate the parameters of a severity estimate model.
#'
#' @param object An instance of \linkS4class{SeverityEstimateModel} whose
#' parameters to estimate.
#' @param ... Further optional args that are eventually given to [rstan::stan()]
#' related to fitting.
#'
#' @importFrom methods slot
#' @rdname fit
#' @export
fit.SeverityEstimateModel <- function(object, ...) {
  # Input validations
  check_model(object)
  # Validations on priors
  parameter_list <- list(
    "active" = c("alpha" = 1.0, "beta" = 1.0),
    "passive_asymptomatic" = c("alpha" = 1.0, "beta" = 3.0),
    "passive_symptomatic" = c("alpha" = 3.0, "beta" = 1.0)
  )
  for (parameter in c(
    "active",
    "passive_asymptomatic",
    "passive_symptomatic"
  )) {
    prior <- methods::slot(object, paste0(parameter, "_prior"))
    if (length(prior) == 0L) {
      prior <- parameter_list[[parameter]]
      warning(
        "An explicit prior was not provided for '",
        parameter,
        "'. The uninformative default of Beta(",
        prior["alpha"],
        ", ",
        prior["beta"],
        ") will be used."
      )
    }
    parameter_list[[parameter]] <- prior
  }
}
