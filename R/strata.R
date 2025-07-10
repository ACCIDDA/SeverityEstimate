#' @title
#' Add A Stratification For IFR/SIR Estimates
#'
#' @inheritParams prior
#' @param name The name of the strataification column, must be present in both
#' the `linelist` and `population` `data.frame`s given when initializing the
#' model.
#' @param levels The levels for the stratification or if `NULL` inferred from
#' the `linelist`/`population` `data.frame`s.
#' @param ordered Indicator for if the levels are ordered in affect, i.e. age
#' increasing severity. If `TRUE` then `levels` must be provided.
#'
#' @return
#' The `model` given modified to contain the stratification information.
#'
#' @examples
#' line_list <- data.frame(
#'   patient = 1L:3L,
#'   week = c(1L, 1L, 2L),
#'   age = c("Youth", "Adult", "Senior"),
#'   detection = c("Active", "Passive", "Active"),
#'   outcome = c("Asymptomatic", "Death", "Symptomatic")
#' )
#' population <- data.frame(
#'   age = c("Youth", "Adult", "Senior"),
#'   amount = rep(987L, 3L)
#' )
#' model <- SeverityEstimateModel(line_list, population) |>
#'   strata("age", levels = c("Youth", "Adult", "Senior"))
#' model
#'
#' @export
strata <- function(model, name, levels = NULL, ordered = FALSE) {
  check_model(model, attribute = "strata", override_warning = FALSE)
  levels <- infer_levels(
    model,
    name,
    "both",
    levels = levels,
    ordered = ordered
  )
  length_plus1 <- length(model@strata) + 1L
  idx <- match(name, sapply(model@strata, \(x) x$name), nomatch = length_plus1)
  if (idx < length_plus1) {
    warning(
      "The given 'model' has a strata called '",
      name,
      "' which has already been set. ",
      "The previously set value will be overridden."
    )
  }
  model@strata[[idx]] <- list(
    "name" = name,
    "levels" = levels,
    "ordered" = ordered
  )
  model
}
