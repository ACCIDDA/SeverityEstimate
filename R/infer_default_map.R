#' @title
#' Infer Default Mappings For Formatted Model Inputs
#'
#' @description
#' Internal helpers used by [default_model()] to infer the detection and outcome
#' mappings required by the model from already-formatted line list values.
#'
#' `infer_default_map()` is the generic validator and mapper. It lowercases the
#' observed values, checks that they can all be interpreted using a supplied
#' lookup table, and returns a named character vector from the original raw
#' values to the canonical model labels.
#'
#' `infer_default_detection_map()` specializes this for the `detection` column,
#' accepting case-insensitive forms of `active`/`passive` and the abbreviations
#' `a`/`p`. If only one detection type has been observed, the returned map is
#' completed with the corresponding unobserved active/passive level so sparse
#' early-outbreak data can still create both surveillance dimensions.
#'
#' `infer_default_outcome_map()` specializes this for the `outcome` column,
#' accepting case-insensitive forms of `asymptomatic`/`symptomatic`/`death` and
#' the abbreviations `a`/`s`/`d`.
#'
#' These helpers are not exported, but they are documented to make the package's
#' data assumptions explicit for contributors extending the model-construction
#' workflow.
#'
#' @param values A vector of observed raw values to map. For the specialized
#' helpers, these are the raw values from the `detection` or `outcome` column.
#' @param valid_map A named character vector of lowercase input values to
#' canonical model values.
#' @param value_name The user-facing name of the values being validated.
#' @param required_types Canonical values that must be represented in the
#' inferred map.
#' @param map A named character vector mapping raw detection values to canonical
#' `active`/`passive` labels.
#' @param type A canonical detection type to create an unobserved raw label for.
#' @param existing_levels Existing raw detection levels whose style should be
#' used when creating the missing level.
#' @param value A detection label whose case should be adjusted.
#' @param template A detection label that provides the desired case style.
#'
#' @returns
#' * `infer_default_map()`, `infer_default_detection_map()`,
#'   `infer_default_outcome_map()`, and `complete_default_detection_map()`
#'   return a named character vector mapping raw values to canonical model
#'   values.
#' * `default_detection_level()` returns a length-one character vector
#'   containing the inferred raw label for an unobserved detection type.
#' * `match_detection_case()` returns `value` converted to match the case style
#'   of `template`.
#'
#' @keywords internal
infer_default_map <- function(
  values,
  valid_map,
  value_name,
  required_types = character()
) {
  values <- as.character(values)
  checkmate::assert_atomic(values, any.missing = FALSE, min.len = 1L)

  unique_values <- unique(values)
  normalized_values <- tolower(unique_values)
  invalid_values <- unique_values[!normalized_values %in% names(valid_map)]
  if (length(invalid_values)) {
    stop(
      "The `",
      value_name,
      "` column contains values that `default_model()` cannot map: ",
      toString(invalid_values),
      ".",
      call. = FALSE
    )
  }

  map <- unname(valid_map[normalized_values])
  names(map) <- unique_values

  missing_types <- setdiff(required_types, unique(map))
  if (length(missing_types)) {
    stop(
      "The `",
      value_name,
      "` column must include values mapping to: ",
      toString(required_types),
      ". Missing: ",
      toString(missing_types),
      ".",
      call. = FALSE
    )
  }

  map
}

#' @rdname infer_default_map
infer_default_detection_map <- function(values) {
  map <- infer_default_map(
    values = values,
    valid_map = c(
      "active" = "active",
      "a" = "active",
      "passive" = "passive",
      "p" = "passive"
    ),
    value_name = "detection"
  )

  complete_default_detection_map(map)
}

#' @rdname infer_default_map
infer_default_outcome_map <- function(values) {
  infer_default_map(
    values = values,
    valid_map = c(
      "asymptomatic" = "asymptomatic",
      "a" = "asymptomatic",
      "symptomatic" = "symptomatic",
      "s" = "symptomatic",
      "death" = "severe",
      "d" = "severe"
    ),
    value_name = "outcome"
  )
}

#' @rdname infer_default_map
complete_default_detection_map <- function(map) {
  missing_types <- setdiff(c("active", "passive"), unique(map))
  for (type in missing_types) {
    map[[default_detection_level(type, names(map))]] <- type
  }
  map
}

#' @rdname infer_default_map
default_detection_level <- function(type, existing_levels) {
  other_type <- if (identical(type, "active")) "passive" else "active"
  other_abbreviation <- substr(other_type, 1L, 1L)
  type_abbreviation <- substr(type, 1L, 1L)

  normalized_levels <- tolower(existing_levels)
  abbreviation_match <- match(other_abbreviation, normalized_levels)
  if (!is.na(abbreviation_match)) {
    return(match_detection_case(
      type_abbreviation,
      existing_levels[[abbreviation_match]]
    ))
  }

  word_match <- match(other_type, normalized_levels)
  if (!is.na(word_match)) {
    return(match_detection_case(type, existing_levels[[word_match]]))
  }

  if (identical(type, "active")) "Active" else "Passive"
}

#' @rdname infer_default_map
match_detection_case <- function(value, template) {
  if (identical(template, toupper(template))) {
    return(toupper(value))
  }
  if (identical(template, tolower(template))) {
    return(tolower(value))
  }
  paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
}
