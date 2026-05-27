#' @title
#' Validate A Mapping Of Values From A Line List
#'
#' @param model A \linkS4class{SeverityEstimateModel} S4 object instance
#' representing a model to check.
#' @param name A column name to check for in either the `line_list` slot of
#' `model`.
#' @param map A named character vector mapping column values to valid types.
#' @param valid_types A character vector of valid types that map values must be.
#' @param required_types A character vector of valid types that map values must
#' include.
#'
#' @return
#' `NULL`, if there are no issues.
#'
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @keywords internal
validate_map <- function(
  model,
  name,
  map,
  valid_types,
  required_types = character()
) {
  checkmate::assert_string(name)
  checkmate::assert_choice(name, names(model@line_list))
  checkmate::assert_character(map, any.missing = FALSE, min.len = 1L)
  checkmate::assert_names(names(map), type = "unique")
  checkmate::assert_character(valid_types, any.missing = FALSE, min.len = 1L)
  checkmate::assert_character(required_types, any.missing = FALSE)
  checkmate::assert_subset(required_types, valid_types)
  if (!all(map %in% valid_types)) {
    invalid_values <- unique(map[!map %in% valid_types])
    stop(
      "Assertion on 'map' failed: All values must be one of ",
      paste0("'", valid_types, "'", collapse = ", "),
      ". Invalid values: ",
      paste(invalid_values, collapse = ", ")
    )
  }
  column_values <- unique(model@line_list[, name, drop = TRUE])
  missing_column_values <- setdiff(column_values, names(map))
  if (length(missing_column_values)) {
    stop(
      "The `",
      name,
      "` map must cover all observed values. Missing: ",
      toString(missing_column_values),
      ".",
      call. = FALSE
    )
  }
  missing_types <- setdiff(required_types, unique(map))
  if (length(missing_types)) {
    stop(
      "The `",
      name,
      "` map must include values for: ",
      toString(required_types),
      ". Missing: ",
      toString(missing_types),
      ".",
      call. = FALSE
    )
  }
  NULL
}
