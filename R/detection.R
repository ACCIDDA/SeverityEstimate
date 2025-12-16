#' @title
#' Add A Detection Type Mapping To A Model
#'
#' @inheritParams prior
#' @param name The name of the detection column, must be present in the `linelist`
#' `data.frame` given when initializing the model.
#' @param map A named character vector mapping detection column values to either
#' "active" or "passive". The names must be values present in the detection column,
#' and the values must be either "active" or "passive".
#'
#' @return
#' The `model` given modified to contain the detection type mapping.
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
#'   detection("detection", map = c("Active" = "active", "Passive" = "passive"))
#' model
#'
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_names
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @export
detection <- function(
  model,
  name,
  map = c("active" = "active", "passive" = "passive")
) {
  check_model(model, attribute = "detection", override_warning = FALSE)

  checkmate::assert_string(name)
  checkmate::assert_choice(name, names(model@line_list))
  checkmate::assert_character(map, any.missing = FALSE, min.len = 1L)
  checkmate::assert_names(names(map), type = "unique")
  checkmate::assert_subset(unname(map), c("active", "passive"))

  detection_values <- unique(model@line_list[, name, drop = TRUE])
  checkmate::assert_subset(names(map), detection_values)

  if (length(model@detection)) {
    old_name <- model@detection$name
    warning(
      "The given 'model' has detection already set to '",
      old_name,
      "'. ",
      "The previously set value will be overridden."
    )
  }

  model@detection <- list(
    "name" = name,
    "map" = map
  )
  model
}
