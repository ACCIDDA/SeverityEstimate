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
#' @importFrom checkmate assert_choice
#' @importFrom methods is
#' @export
time <- function(model, name, levels = NULL) {
  stopifnot(is(model, "SeverityEstimateModel"))
  checkmate::assert_choice(name, names(model@linelist))
  inferred_levels <- sort(unique(model@linelist[, name, drop = TRUE]))
  if (is.null(levels)) {
    levels <- inferred_levels
  }
  checkmate::assert_choice(inferred_levels, levels)
  model@time <- list("name" = name, "levels" = levels)
  model
}
