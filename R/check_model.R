#' @title
#' Check a `SeverityEstimateModel` instance
#'
#' @description
#' Perform common checks on a `SeverityEstimateModel` instances, namely that it
#' is an instance of that class and has the attributes needed for the particular
#' function interacting with it.
#'
#' @param model A \linkS4class{SeverityEstimateModel} S4 object instance
#' representing a model to check.
#' @param attribute The attribute being modified by the function calling this
#' check. If this attribute is set a warning will be issued to the user letting
#' them know.
#' @param override_warning Whether there should be a warning if `attribute` is
#' already set. This function only checks the length of the attribute to
#' determine if it is set.
#'
#' @return
#' `check_model` returns `NULL` and will raise an error if there is an issue.
#'
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_string
#' @importFrom methods slot
#' @importFrom methods slotNames
#' @keywords internal
check_model <- function(
  model,
  attribute = NULL,
  override_warning = TRUE
) {
  checkmate::assert_class(model, "SeverityEstimateModel")
  checkmate::assert_string(attribute, null.ok = TRUE)
  if (!is.null(attribute)) {
    checkmate::assert_choice(attribute, methods::slotNames(model))
    if (override_warning) {
      slot_value <- methods::slot(model, attribute)
      if (length(slot_value)) {
        warning(
          "The given 'model' has an attribute called '",
          attribute,
          "' which has already been set. ",
          "The previously set value will be overridden."
        )
      }
    }
  }
  NULL
}
