#' @title
#' Construct A Default Severity Model
#'
#' @description
#' Construct a \linkS4class{SeverityEstimateModel} from already-formatted line
#' list and population data by inferring the model structure from column names.
#'
#' The `line_list` must contain `time`, `detection`, and `outcome` columns.
#' Every other `line_list` column is treated as a strata column with
#' `degrees_of_freedom = 0L`, so those columns must also be present in
#' `population`. The `population` data must then contain exactly one additional
#' non-strata column, which is treated as the population count column.
#'
#' Detection values must be case-insensitive forms of `active`/`passive` or
#' `a`/`p`. Outcome values must be case-insensitive forms of
#' `asymptomatic`/`symptomatic`/`death` or `a`/`s`/`d`.
#'
#' The returned model includes weakly informative detection priors suitable for
#' fitting immediately with [fit()].
#'
#' @param line_list A `data.frame` (or `data.frame` extending object like a
#' `tibble`) of line list data.
#' @param population A `data.frame` (or `data.frame` extending object like a
#' `tibble`) of population data.
#'
#' @returns
#' A \linkS4class{SeverityEstimateModel} S4 object instance.
#'
#' @examples
#' line_list <- data.frame(
#'   time = c(1L, 1L, 2L),
#'   age = c("Youth", "Adult", "Senior"),
#'   detection = c("Active", "Passive", "Active"),
#'   outcome = c("Asymptomatic", "Death", "Symptomatic")
#' )
#' population <- data.frame(
#'   age = c("Youth", "Adult", "Senior"),
#'   value = c(1000L, 1200L, 900L)
#' )
#' model <- default_model(line_list, population)
#' model
#'
#' @export
default_model <- function(line_list, population) {
  required_line_list_cols <- c("time", "detection", "outcome")
  line_list <- is_data_frame(
    line_list,
    has_string_columns = c("detection", "outcome")
  )
  population <- is_data_frame(population)

  missing_line_list_cols <- setdiff(required_line_list_cols, names(line_list))
  if (length(missing_line_list_cols)) {
    stop(
      "`line_list` is missing required columns: ",
      toString(missing_line_list_cols),
      ".",
      call. = FALSE
    )
  }

  strata_cols <- setdiff(names(line_list), required_line_list_cols)
  missing_population_cols <- setdiff(strata_cols, names(population))
  if (length(missing_population_cols)) {
    stop(
      "`population` is missing inferred strata columns: ",
      toString(missing_population_cols),
      ".",
      call. = FALSE
    )
  }

  population_value_cols <- setdiff(names(population), strata_cols)
  if (length(population_value_cols) != 1L) {
    stop(
      "`population` must contain exactly one non-strata column ",
      "representing the population counts. Found: ",
      toString(population_value_cols),
      ".",
      call. = FALSE
    )
  }
  population_value_col <- population_value_cols[[1L]]
  population <- population[, c(strata_cols, population_value_col), drop = FALSE]

  model <- SeverityEstimateModel(line_list, population) |>
    set_timesteps("time") |>
    set_detection(
      "detection",
      map = infer_default_detection_map(line_list[, "detection", drop = TRUE])
    ) |>
    set_outcome(
      "outcome",
      map = infer_default_outcome_map(line_list[, "outcome", drop = TRUE])
    ) |>
    set_active_prior(alpha = 1.0, beta = 1.0) |>
    set_passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
    set_passive_symptomatic_prior(alpha = 3.0, beta = 1.0)

  if (!length(strata_cols)) {
    return(model)
  }

  for (name in strata_cols) {
    model <- model |>
      set_strata(name, degrees_of_freedom = 0L)
  }

  model
}
