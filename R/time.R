#' @title
#' Add A Time Specification To A Model
#'
#' @inheritParams prior
#' @param name The name of the time column, must be present in the `line_list`
#' `data.frame` given when initializing the model.
#' @param levels The levels for the time or if `NULL` inferred from the
#' `line_list` `data.frame`. Providing this explicitly is strongly recommended
#' to ensure the order is as expected by the user.
#'
#' @return
#' The `model` given modified to contain the time information.
#'
#' @export
time <- function(model, name, levels = NULL) {
  check_model(model, attribute = "time")
  levels <- infer_levels(model, name, "line_list", levels = levels)
  model@time <- list("name" = name, "levels" = levels)
  model
}
