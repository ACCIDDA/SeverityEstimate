#' @title
#' Get Or Set Model Timesteps
#'
#' @description
#' S4 getter and replacement methods for the `timesteps` slot on a
#' \linkS4class{SeverityEstimateModel}, plus a chainable `set_timesteps()`
#' helper for pipeline ergonomics.
#'
#' @param x A \linkS4class{SeverityEstimateModel}.
#' @param value A named list with entries `name` and `levels`.
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param name The name of the timestep column, which must be present in
#' `line_list`.
#' @param levels The levels for the timestep, or `NULL` to infer from
#' `line_list`.
#' @param mode How `require_timesteps()` should respond when timesteps are not
#' set. One of `error`, `warn`, `silent`.
#'
#' @return
#' `timesteps(x)` returns the current timestep specification.
#'
#' `timesteps(x) <- value` returns `x` modified to include the timestep
#' specification.
#'
#' `has_timesteps(x)` returns `TRUE` if timesteps have been set, `FALSE`
#' otherwise.
#'
#' `require_timesteps(model, mode)` returns `model`. If timesteps are unset, the
#' behavior depends on `mode`.
#'
#' `set_timesteps(model, ...)` returns `model` modified to include the
#' timestep specification.
#'
#' @importFrom methods setGeneric
#' @rdname timesteps
#' @export
methods::setGeneric(
  "timesteps",
  function(x) standardGeneric("timesteps")
)

#' @importFrom methods setGeneric
#' @rdname timesteps
#' @export
methods::setGeneric(
  "timesteps<-",
  function(x, value) standardGeneric("timesteps<-")
)

#' @importFrom methods setMethod
#' @importFrom methods slot
#' @rdname timesteps
#' @export
methods::setMethod(
  "timesteps",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    methods::slot(x, "timesteps")
  }
)

#' @importFrom methods setGeneric
#' @rdname timesteps
#' @export
methods::setGeneric(
  "has_timesteps",
  function(x) standardGeneric("has_timesteps")
)

#' @rdname timesteps
#' @export
methods::setMethod(
  "has_timesteps",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    length(methods::slot(x, "timesteps")) > 0L
  }
)

#' @rdname timesteps
#' @export
methods::setMethod(
  "timesteps<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (!is.list(value) || is.null(value[["name"]])) {
      stop(
        "The replacement value for 'timesteps' must be a list with a 'name'."
      )
    }
    name <- value[["name"]]
    levels <- value[["levels"]]
    check_model(x, attribute = "timesteps", override_warning = FALSE)
    if (length(x@timesteps)) {
      old_name <- x@timesteps$name
      warning(
        "The given 'model' has timesteps already set to '",
        old_name,
        "'. ",
        "The previously set value will be overridden."
      )
    }
    x@timesteps <- list(
      "name" = name,
      "levels" = infer_levels(x, name, "line_list", levels = levels)
    )
    x
  }
)

#' @importFrom checkmate assert_choice
#' @rdname timesteps
#' @export
require_timesteps <- function(model, mode = "error") {
  check_model(model)
  checkmate::assert_choice(mode, c("error", "warn", "silent"))
  if (!has_timesteps(model)) {
    msg <- "No timesteps have been set. Call `set_timesteps(...)` first."
    if (mode == "error") {
      stop(msg, call. = FALSE)
    }
    if (mode == "warn") {
      warning(msg, call. = FALSE)
    }
  }
  model
}

#' @rdname timesteps
#' @export
set_timesteps <- function(model, name, levels = NULL) {
  timesteps(model) <- list(name = name, levels = levels)
  model
}
