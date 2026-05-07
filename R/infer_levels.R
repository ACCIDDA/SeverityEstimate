#' @title
#' Infer And Validate Levels For Variables
#'
#' @param model A \linkS4class{SeverityEstimateModel} S4 object instance
#' representing a model to check.
#' @param name A column name to check for in either the `line_list` and/or
#' `population` slots of `model`.
#' @param name_in A string indication which attribute to check `name` against.
#' Must be 'line_list', 'population', or 'both'.
#' @param levels Explicit user provided levels, in order, if provided otherwise
#' `NULL` to infer the levels.
#'
#' @return
#' `infer_levels` returns a vector of levels for the particular column given.
#' Either `levels` if non-`NULL` or an inferred set of levels.
#'
#' Explicit `levels` may not contain missing or duplicated values. Likewise,
#' missing values in the source column are rejected before inferred levels are
#' constructed.
#'
#' @importFrom checkmate assert_atomic
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @keywords internal
infer_levels <- function(model, name, name_in, levels = NULL) {
  # Validations
  checkmate::assert_string(name)
  checkmate::assert_choice(name_in, c("line_list", "population", "both"))
  if (name_in %in% c("line_list", "both")) {
    checkmate::assert_choice(name, names(model@line_list))
  }
  if (name_in %in% c("population", "both")) {
    checkmate::assert_choice(name, names(model@population))
  }
  if (!is.null(levels)) {
    checkmate::assert_atomic(levels, any.missing = FALSE)
    if (anyDuplicated(levels)) {
      stop(
        "Assertion on 'levels' failed: Values must be unique.",
        call. = FALSE
      )
    }
  }
  if (name_in == "line_list") {
    inferred_levels <- model@line_list[, name, drop = TRUE]
  } else if (name_in == "population") {
    inferred_levels <- model@population[, name, drop = TRUE]
  } else {
    inferred_levels <- c(
      model@line_list[, name, drop = TRUE],
      model@population[, name, drop = TRUE]
    )
  }
  checkmate::assert_atomic(
    inferred_levels,
    any.missing = FALSE,
    .var.name = "levels"
  )
  inferred_levels <- sort(unique(inferred_levels))
  if (is.null(levels)) {
    levels <- inferred_levels
  }
  checkmate::assert_subset(inferred_levels, levels)
  levels
}
