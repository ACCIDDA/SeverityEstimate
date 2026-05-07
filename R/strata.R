#' @title
#' Get Or Set Model Stratifications
#'
#' @description
#' S4 getter and replacement methods for the `strata` slot on a
#' \linkS4class{SeverityEstimateModel}, plus a chainable `set_strata()` helper
#' for pipeline ergonomics.
#'
#' @param x A \linkS4class{SeverityEstimateModel}.
#' @param value A named list with entries `name`, `levels`, and
#' `degrees_of_freedom`.
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param name The name of the stratification column, which must be present in
#' both the `line_list` and `population` `data.frame`s.
#' @param levels The levels for the stratification, or `NULL` to infer from
#' `line_list`/`population`.
#' @param degrees_of_freedom The degrees of freedom for the strata fixed
#' effects. `NULL` and `0L` use unsmoothed categorical effects. Values greater
#' than `0L` request an ordered smooth effect and therefore require explicit
#' `levels`. The value must be less than the saturated categorical fit, i.e.
#' at most `length(levels) - 2L`.
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
    check_model(x, attribute = "strata", override_warning = FALSE)
    degrees_of_freedom <- value[["degrees_of_freedom"]]
    checkmate::assert_integerish(
      degrees_of_freedom,
      len = 1L,
      lower = 0L,
      null.ok = TRUE
    )
    if (is.null(degrees_of_freedom)) {
      degrees_of_freedom <- 0L
    }
    degrees_of_freedom <- as.integer(degrees_of_freedom)
    if (degrees_of_freedom > 0L && is.null(levels)) {
      stop(
        "Assertion on 'levels' failed: Explicit levels must be provided ",
        "when `degrees_of_freedom > 0L`.",
        call. = FALSE
      )
    }
    levels <- infer_levels(
      x,
      name,
      "both",
      levels = levels
    )
    if (degrees_of_freedom > 0L) {
      n_levels <- length(levels)
      if (n_levels < 3L) {
        stop(
          "Assertion on 'levels' failed: Smoothed strata require at least ",
          "3 levels.",
          call. = FALSE
        )
      }
      if (degrees_of_freedom > (n_levels - 2L)) {
        stop(
          "Assertion on 'degrees_of_freedom' failed: Must be at most ",
          n_levels - 2L,
          " for ",
          n_levels,
          " levels. Use `degrees_of_freedom = 0L` for an unsmoothed ",
          "categorical effect.",
          call. = FALSE
        )
      }
    }
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
  degrees_of_freedom = NULL
) {
  strata(model) <- list(
    name = name,
    levels = levels,
    degrees_of_freedom = degrees_of_freedom
  )
  model
}
