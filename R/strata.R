#' @title
#' Add A Stratification For IFR/SIR Estimates
#'
#' @inheritParams prior
#' @param name The name of the strataification column, must be present in both
#' the `linelist` and `population` `data.frame`s given when initializing the
#' model.
#' @param levels The levels for the stratification or if `NULL` inferred from
#' the `linelist`/`population` `data.frame`s.
#' @param ordered Indicator for if the levels are ordered in affect, i.e. age
#' increasing severity. If `TRUE` then `levels` must be provided.
#'
#' @return
#' The `model` given modified to contain the stratification information.
#'
#' @importFrom checkmate assert
#' @importFrom checkmate assert_choice
#' @importFrom checkmate check_choice
#' @importFrom checkmate vname
#' @importFrom methods is
#' @export
strata <- function(model, name, levels = NULL, ordered = FALSE) {
  stopifnot(is(model, "SeverityEstimateModel"))
  checkmate::assert(
    checkmate::check_choice(name, names(model@linelist)),
    checkmate::check_choice(name, names(model@population)),
    combine = "and"
  )
  assert_bool(ordered)
  inferred_levels <- sort(unique(c(
    model@linelist[, name, drop = TRUE],
    model@population[, name, drop = TRUE]
  )))
  if (is.null(levels)) {
    if (ordered) {
      stop(
        "Assertion on 'levels' failed: Explicit levels ",
        "must be provided when `ordered` is `TRUE`."
      )
    }
    levels <- inferred_levels
  }
  checkmate::assert_choice(inferred_levels, levels)
  model@strata <- append(
    model@strata,
    list("name" = name, "levels" = levels, "ordered" = ordered)
  )
  model
}
