#' @title
#' Run A Stan Model From A Template
#'
#' @inheritParams render_template
#' @param fun The function to call on the rendered template, typically one of
#' [rstan::stan()] for live use and [rstan::stanc()] for unit testing.
#' @param ... Further arguments passed to the `fun` function.
#'
#' @returns
#' The output of `fun` called with the given further arguments.
#'
#' @importFrom rstan stan
#' @keywords internal
stan_model <- function(
  template,
  template_data = list(),
  fun = rstan::stan,
  ...
) {
  args <- list(...)
  args[["model_code"]] <- render_template(
    template,
    template_data = template_data
  )
  if (!"model_name" %in% names(args)) {
    args[["model_name"]] <- template
  }
  do.call(fun, args)
}
