#' @title
#' Run A Precompiled Stan Model
#'
#' @param model_name The name of a model in `stanmodels`.
#' @param ... Further arguments passed to [rstan::sampling()].
#'
#' @returns
#' The output of [rstan::sampling()] called with the given further arguments.
#'
#' @importFrom rstan sampling
#' @noRd
stan_model <- function(model_name, ...) {
  model <- stanmodels[[model_name]]
  if (is.null(model)) {
    stop("Unknown Stan model: '", model_name, "'.", call. = FALSE)
  }

  args <- list(...)
  if ("object" %in% names(args)) {
    stop("Do not pass `object` to `stan_model()`.", call. = FALSE)
  }
  args[["object"]] <- model
  do.call(rstan::sampling, args)
}
