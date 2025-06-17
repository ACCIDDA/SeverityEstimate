#' @title
#' Add A Time Specification To A Model
#'
#' @inheritParams prior
#' @param name The name of the time column, must be present in the `linelist`
#' `data.frame` given when initializing the model.
#' @param levels The levels for the time or if `NULL` inferred from the
#' `linelist` `data.frame`. Providing this explicitly is strongly recommended
#' to ensure the order is as expected by the user.
#'
#' @return
#' The `model` given modified to contain the time information.
#'
#' @importFrom methods is
#' @export
time <- function(model, name, levels = NULL) {
  stopifnot(is(model, "SeverityEstimateModel"))
  if (length(model@time)) {
    old_name <- model@time$name
    warning(
      "The given 'model' has time already set to '",
      old_name,
      "'. ",
      "The previously set value will be overridden."
    )
  }
  model@time <- list(
    "name" = name,
    "levels" = infer_levels(model, name, "line_list", levels = levels)
  )
  model
}
