#' @title
#' Get Or Set Model Stratifications
#'
#' @description
#' S4 getter and replacement methods for the `strata` slot on a
#' \linkS4class{SeverityEstimateModel}, plus a chainable `set_strata()` helper
#' for pipeline ergonomics.
#'
#' @param x A \linkS4class{SeverityEstimateModel}.
#' @param value A named list with entries `name`, `levels`, `ordered`, and
#' `degrees_of_freedom`.
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param name The name of the stratification column, which must be present in
#' both the `line_list` and `population` `data.frame`s.
#' @param levels The levels for the stratification, or `NULL` to infer from
#' `line_list`/`population`.
#' @param ordered Indicator for if the levels are ordered in effect, i.e. age
#' increasing severity. If `TRUE` then `levels` must be provided. Currently
#' must be `FALSE`; ordered strata are not yet supported.
#' @param degrees_of_freedom The degrees of freedom for the strata fixed
#' effects. Only used when `ordered` is `FALSE`. If `NULL`, defaults to `1L`
#' with a warning.
#'
#' @return
#' `strata(x)` returns the current list of model stratifications.
#'
#' `strata(x) <- value` returns `x` modified to include the given
#' stratification.
#'
#' `set_strata(model, ...)` returns `model` modified to include the given
#' stratification.
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
#'   set_strata("age", levels = c("Youth", "Adult", "Senior"))
#' model
#'
#' @importFrom methods setGeneric
#' @rdname strata
#' @export
methods::setGeneric(
  "strata",
  function(x) standardGeneric("strata")
)

#' @importFrom methods setGeneric
#' @rdname strata
#' @export
methods::setGeneric(
  "strata<-",
  function(x, value) standardGeneric("strata<-")
)

#' @importFrom methods setMethod
#' @importFrom methods slot
#' @rdname strata
#' @export
methods::setMethod(
  "strata",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    methods::slot(x, "strata")
  }
)

#' @rdname strata
#' @export
methods::setMethod(
  "strata<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (!is.list(value) || is.null(value[["name"]])) {
      stop("The replacement value for 'strata' must be a list with a 'name'.")
    }
    name <- value[["name"]]
    levels <- value[["levels"]]
    ordered <- value[["ordered"]]
    if (is.null(ordered)) {
      ordered <- FALSE
    }
    check_model(x, attribute = "strata", override_warning = FALSE)
    degrees_of_freedom <- value[["degrees_of_freedom"]]
    if (is.null(degrees_of_freedom)) {
      warning(
        "No `degrees_of_freedom` specified for strata '",
        name,
        "'. Defaulting to 1L.",
        call. = FALSE
      )
      degrees_of_freedom <- 1L
    }
    degrees_of_freedom <- as.integer(degrees_of_freedom)
    levels <- infer_levels(
      x,
      name,
      "both",
      levels = levels,
      ordered = ordered
    )
    length_plus1 <- length(x@strata) + 1L
    idx <- match(name, sapply(x@strata, \(s) s$name), nomatch = length_plus1)
    if (idx < length_plus1) {
      warning(
        "The given 'model' has a strata called '",
        name,
        "' which has already been set. ",
        "The previously set value will be overridden."
      )
    }
    x@strata[[idx]] <- list(
      "name" = name,
      "levels" = levels,
      "ordered" = ordered,
      "degrees_of_freedom" = degrees_of_freedom
    )
    x
  }
)

#' @rdname strata
#' @export
set_strata <- function(
  model,
  name,
  levels = NULL,
  ordered = FALSE,
  degrees_of_freedom = NULL
) {
  strata(model) <- list(
    name = name,
    levels = levels,
    ordered = ordered,
    degrees_of_freedom = degrees_of_freedom
  )
  model
}
