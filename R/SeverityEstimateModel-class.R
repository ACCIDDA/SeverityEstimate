#' @title
#' Severity Estimate Model
#'
#' @description
#' A representation of a severity estimate model and its metadata.
#'
#' @slot line_list A line list of cases to model the severity of.
#' @slot population A dataset containing information on the population broken
#' down by strataification.
#'
#' @importFrom methods setClass
#' @export
#' @rdname SeverityEstimateModel
setClass(
  Class = "SeverityEstimateModel",
  slots = c(
    "line_list" = "data.frame",
    "population" = "data.frame"
  ),
  prototype = list(
    "line_list" = data.frame(),
    "population" = data.frame()
  )
)


#' @title
#' Create A Severity Model Instance
#'
#' @param line_list A line list of cases to model the severity of.
#' @param population A dataset containing information on the population broken
#' down by strataification.
#'
#' @return
#' A \linkS4class{SeverityEstimateModel} S4 object instance representing a model
#' and its associated metadata.
#'
#' @importFrom methods new
#' @export
SeverityEstimateModel <- function(line_list, population) {
  line_list <- is_data_frame(line_list)
  population <- is_data_frame(population)
  methods::new(
    "SeverityEstimateModel",
    line_list = line_list,
    population = population
  )
}


#' @title
#' Print Method for `SeverityEstimateModel` Objects
#'
#' @description
#' Prints a \linkS4class{SeverityEstimateModel} object in a structured format.
#' Currently just a thin wrapper around [utils::str()].
#'
#' @param x An object of class \linkS4class{SeverityEstimateModel}.
#' @param ... Further arguments passed to the [utils::str()] function.
#'
#' @return
#' `x` invisibly.
#'
#' @importFrom utils str
#' @export
print.SeverityEstimateModel <- function(x, ...) {
  # For now just fallback to stan's print method
  utils::str(x, ...)
  invisible(x)
}


#' @title
#' Perform Commonly Repeated Checks For Operations On Models
#'
#' @description
#' These helpers consolidate common validations and operations on models for use
#' by functions that operate on models like [strata()] or [time()].
#'
#' @param model A \linkS4class{SeverityEstimateModel} S4 object instance
#' representing a model to check.
#' @param attribute The attribute being modified by the function calling this
#' check. If this attribute is set a warning will be issued to the user letting
#' them know.
#' @param name A column name to check for in either the `line_list` and/or
#' `population` slots of `model`.
#' @param name_in A string indication which attribute to check `name` against.
#' Must be 'line_list', 'population', or 'both'.
#' @param ordered A boolean indicating if `levels` has a specific order. If
#' `TRUE` then `levels` cannot be `NULL`, users must explicitly indicate what
#' the ordering is.
#'
#' @details
#' When using this functionality it is expected that `check_model` is always
#' called first so subsequent helpers will skip validations performed by this
#' function.
#'
#' @return
#' `check_model` returns `NULL` and will raise an error if there is an issue.
#'
#' `infer_levels` returns a vector of levels for the particular column given.
#' Either `levels` if non-`NULL` or an inferred set of levels.
#'
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_subset
#' @importFrom checkmate assert_string
#' @importFrom methods slot
#' @importFrom methods slotNames
#' @keywords internal
check_model <- function(
  model,
  attribute = NULL,
  name = NULL,
  name_in = NULL,
  ordered = NA
) {
  checkmate::assert_class(model, "SeverityEstimateModel")
  checkmate::assert_string(attribute, null.ok = TRUE)
  if (!is.null(attribute)) {
    checkmate::assert_choice(attribute, methods::slotNames(model))
    if (length(methods::slot(model, attribute))) {
      warning(
        "The given 'model' has an attribute called '",
        attribute,
        "' which has already been set. ",
        "The previously set value will be overridden."
      )
    }
  }
  checkmate::assert_string(name, null.ok = TRUE)
  if (!is.null(name)) {
    checkmate::assert_choice(name_in, c("line_list", "population", "both"))
    if (name_in %in% c("line_list", "both")) {
      checkmate::assert_choice(name, names(model@line_list))
    }
    if (name_in %in% c("population", "both")) {
      checkmate::assert_choice(name, names(model@population))
    }
  }
  assert_bool(ordered, na.ok = TRUE)
  NULL
}
