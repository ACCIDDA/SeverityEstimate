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
#' @export
strata <- function(model, name, levels = NULL, ordered = FALSE) {
  check_model(model)
  levels <- infer_levels(
    model,
    name,
    "both",
    levels = levels,
    ordered = ordered
  )
  model@strata <- append(
    model@strata,
    list("name" = name, "levels" = levels, "ordered" = ordered)
  )
  model
}
