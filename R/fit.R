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
#' @importFrom generics fit
#' @importFrom methods slot
#' @method fit SeverityEstimateModel
#' @rdname fit
#' @export
setMethod(
  "fit",
  signature = "SeverityEstimateModel",
  definition = function(object, ...) {
    # Input validations
    check_model(object)
    # Validations on priors
    data <- list(
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
        prior <- data[[parameter]]
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
      data[[parameter]] <- prior
    }
    # Extract the time
    if (length(object@time) == 0L) {
      stop(
        "The time specification has not been described to this model yet. ",
        "Did you call the `SeverityEstimate::time` function on this model?"
      )
    }
    data[["n_time"]] <- length(object@time$levels)
    data[["ll_time_index"]] <- match(
      object@line_list[, object@time$name],
      object@time$levels
    )
  }
)
