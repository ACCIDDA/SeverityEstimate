#' @title
#' Get Or Set Model Outcome Mapping
#'
#' @description
#' S4 getter and replacement methods for the `outcome` slot on a
#' \linkS4class{SeverityEstimateModel}, plus chainable helpers to set and
#' validate outcome readiness.
#'
#' @param x A \linkS4class{SeverityEstimateModel}.
#' @param value A named list with entries `name` and `map`.
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param name The name of the outcome column, which must be present in
#' `line_list`.
#' @param map A named character vector mapping outcome column values to one of
#' `asymptomatic`, `symptomatic`, or `severe`.
#' @param mode How `require_outcome()` should respond when outcome is not set.
#' One of `error`, `warn`, `silent`.
#'
#' @return
#' `outcome(x)` returns the current outcome specification.
#'
#' `outcome(x) <- value` returns `x` modified to include the outcome
#' specification.
#'
#' `has_outcome(x)` returns `TRUE` if outcome has been set, `FALSE` otherwise.
#'
#' `require_outcome(model, mode)` returns `model`. If outcome is unset, the
#' behavior depends on `mode`.
#'
#' `set_outcome(model, ...)` returns `model` modified to include the outcome
#' specification.
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
#'   set_outcome("outcome", map = c(
#'     "Asymptomatic" = "asymptomatic",
#'     "Symptomatic" = "symptomatic",
#'     "Death" = "severe"
#'   ))
#' model
#'
#' @importFrom methods setGeneric
#' @rdname outcome
#' @export
methods::setGeneric(
  "outcome",
  function(x) standardGeneric("outcome")
)

#' @importFrom methods setGeneric
#' @rdname outcome
#' @export
methods::setGeneric(
  "outcome<-",
  function(x, value) standardGeneric("outcome<-")
)

#' @importFrom methods setMethod
#' @importFrom methods slot
#' @rdname outcome
#' @export
methods::setMethod(
  "outcome",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    methods::slot(x, "outcome")
  }
)

#' @importFrom methods setGeneric
#' @rdname outcome
#' @export
methods::setGeneric(
  "has_outcome",
  function(x) standardGeneric("has_outcome")
)

#' @rdname outcome
#' @export
methods::setMethod(
  "has_outcome",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    length(methods::slot(x, "outcome")) > 0L
  }
)

#' @rdname outcome
#' @export
methods::setMethod(
  "outcome<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (!is.list(value) || is.null(value[["name"]])) {
      stop("The replacement value for 'outcome' must be a list with a 'name'.")
    }
    name <- value[["name"]]
    map <- value[["map"]]
    if (is.null(map)) {
      map <- c(
        "asymptomatic" = "asymptomatic",
        "symptomatic" = "symptomatic",
        "severe" = "severe"
      )
    }

    check_model(x, attribute = "outcome", override_warning = FALSE)
    validate_map(
      x,
      name,
      map,
      valid_types = c("asymptomatic", "symptomatic", "severe")
    )

    if (length(x@outcome)) {
      old_name <- x@outcome$name
      warning(
        "The given 'model' has outcome already set to '",
        old_name,
        "'. ",
        "The previously set value will be overridden."
      )
    }

    x@outcome <- list(
      "name" = name,
      "map" = map
    )
    x
  }
)

#' @importFrom checkmate assert_choice
#' @rdname outcome
#' @export
require_outcome <- function(model, mode = "error") {
  check_model(model)
  checkmate::assert_choice(mode, c("error", "warn", "silent"))
  if (!has_outcome(model)) {
    msg <- "No outcome mapping has been set. Call `set_outcome(...)` first."
    if (mode == "error") {
      stop(msg, call. = FALSE)
    }
    if (mode == "warn") {
      warning(msg, call. = FALSE)
    }
  }
  model
}

#' @rdname outcome
#' @export
set_outcome <- function(
  model,
  name,
  map = c(
    "asymptomatic" = "asymptomatic",
    "symptomatic" = "symptomatic",
    "severe" = "severe"
  )
) {
  outcome(model) <- list(name = name, map = map)
  model
}
