#' @title
#' Get Or Set Model Detection Mapping
#'
#' @description
#' S4 getter and replacement methods for the `detection` slot on a
#' \linkS4class{SeverityEstimateModel}, plus chainable helpers to set and
#' validate detection readiness.
#'
#' @param x A \linkS4class{SeverityEstimateModel}.
#' @param value A named list with entries `name` and `map`.
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param name The name of the detection column, which must be present in
#' `line_list`.
#' @param map A named character vector mapping detection column values to either
#' `active` or `passive`.
#' @param mode How `require_detection()` should respond when detection is not
#' set. One of `error`, `warn`, `silent`.
#'
#' @return
#' `detection(x)` returns the current detection specification.
#'
#' `detection(x) <- value` returns `x` modified to include the detection
#' specification.
#'
#' `has_detection(x)` returns `TRUE` if detection has been set, `FALSE`
#' otherwise.
#'
#' `require_detection(model, mode)` returns `model`. If detection is unset, the
#' behavior depends on `mode`.
#'
#' `set_detection(model, ...)` returns `model` modified to include the detection
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
#'   set_detection(
#'     "detection",
#'     map = c("Active" = "active", "Passive" = "passive")
#'   )
#' model
#'
#' @importFrom methods setGeneric
#' @rdname detection
#' @export
methods::setGeneric(
  "detection",
  function(x) standardGeneric("detection")
)

#' @importFrom methods setGeneric
#' @rdname detection
#' @export
methods::setGeneric(
  "detection<-",
  function(x, value) standardGeneric("detection<-")
)

#' @importFrom methods setMethod
#' @importFrom methods slot
#' @rdname detection
#' @export
methods::setMethod(
  "detection",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    methods::slot(x, "detection")
  }
)

#' @importFrom methods setGeneric
#' @rdname detection
#' @export
methods::setGeneric(
  "has_detection",
  function(x) standardGeneric("has_detection")
)

#' @rdname detection
#' @export
methods::setMethod(
  "has_detection",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    length(methods::slot(x, "detection")) > 0L
  }
)

#' @rdname detection
#' @export
methods::setMethod(
  "detection<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (!is.list(value) || is.null(value[["name"]])) {
      stop(
        "The replacement value for 'detection' must be a list with a 'name'."
      )
    }
    name <- value[["name"]]
    map <- value[["map"]]
    if (is.null(map)) {
      map <- c("active" = "active", "passive" = "passive")
    }

    check_model(x, attribute = "detection", override_warning = FALSE)
    validate_map(x, name, map, valid_types = c("active", "passive"))

    if (length(x@detection)) {
      old_name <- x@detection$name
      warning(
        "The given 'model' has detection already set to '",
        old_name,
        "'. ",
        "The previously set value will be overridden."
      )
    }

    x@detection <- list(
      "name" = name,
      "map" = map
    )
    x
  }
)

#' @importFrom checkmate assert_choice
#' @rdname detection
#' @export
require_detection <- function(model, mode = "error") {
  check_model(model)
  checkmate::assert_choice(mode, c("error", "warn", "silent"))
  if (!has_detection(model)) {
    msg <- "No detection mapping has been set. Call `set_detection(...)` first."
    if (mode == "error") {
      stop(msg, call. = FALSE)
    }
    if (mode == "warn") {
      warning(msg, call. = FALSE)
    }
  }
  model
}

#' @rdname detection
#' @export
set_detection <- function(
  model,
  name,
  map = c("active" = "active", "passive" = "passive")
) {
  detection(model) <- list(name = name, map = map)
  model
}
