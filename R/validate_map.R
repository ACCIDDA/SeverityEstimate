#' @title
#' Validate A Mapping Of Values From A Line List
#'
#' @param model A \linkS4class{SeverityEstimateModel} S4 object instance
#' representing a model to check.
#' @param name A column name to check for in either the `line_list` slot of
#' `model`.
#' @param map A named character vector mapping column values to valid types.
#' @param valid_types A character vector of valid types that map values must be.
#'
#' @return
#' `NULL`, if there are no issues.
#'
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @keywords internal
validate_map <- function(model, name, map, valid_types) {
  checkmate::assert_string(name)
  checkmate::assert_choice(name, names(model@line_list))
  checkmate::assert_character(map, any.missing = FALSE, min.len = 1L)
  checkmate::assert_names(names(map), type = "unique")
  checkmate::assert_character(valid_types, any.missing = FALSE, min.len = 1L)
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
  checkmate::assert_subset(names(map), column_values)
  NULL
}
