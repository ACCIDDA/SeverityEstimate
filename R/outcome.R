#' @title
#' Add An Outcome Severity Mapping To A Model
#'
#' @inheritParams prior
#' @param name The name of the outcome column, must be present in the `linelist`
#' `data.frame` given when initializing the model.
#' @param map A named character vector mapping outcome column values to one of
#' "asymptomatic", "symptomatic", or "severe". The names must be values present
#' in the outcome column, and the values must be one of "asymptomatic",
#' "symptomatic", or "severe".
#'
#' @return
#' The `model` given modified to contain the outcome severity mapping.
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
#'   outcome("outcome", map = c(
#'     "Asymptomatic" = "asymptomatic",
#'     "Symptomatic" = "symptomatic",
#'     "Death" = "severe"
#'   ))
#' model
#'
#' @export
outcome <- function(
  model,
  name,
  map = c(
    "asymptomatic" = "asymptomatic",
    "symptomatic" = "symptomatic",
    "severe" = "severe"
  )
) {
  check_model(model, attribute = "outcome", override_warning = FALSE)
  validate_map(
    model,
    name,
    map,
    valid_types = c("asymptomatic", "symptomatic", "severe")
  )

  if (length(model@outcome)) {
    old_name <- model@outcome$name
    warning(
      "The given 'model' has outcome already set to '",
      old_name,
      "'. ",
      "The previously set value will be overridden."
    )
  }

  model@outcome <- list(
    "name" = name,
    "map" = map
  )
  model
}
