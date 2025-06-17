#' @title
#' Format And Validate A Extract `data.frame`s
#'
#' @description
#' Format the labels of a single column surveillance/outcome `data.frame` for
#' internal use.
#'
#' @param df A `data.frame` (or `data.frame` extending object like a `tibble`)
#' describing the levels.
#' @param name A length one character of a name to provide for error messages.
#' @param valid_levels A list of characters describing sets of valid levels.
#' Each character in this list should be the same length.
#' @param level_converter A function to process the values in `df` before
#' converting to a factor.
#' @param case_insensitive_levels A length one logical indicating if levels are
#' case insensitive.
#'
#' @return
#' A `data.frame` that is similar to the input given, but with a single column
#' as a factor with:
#' * 'Active' and 'Passive' levels for `format_surveillance_data_frame`, and
#' * 'Asymptomatic', 'Death', and 'Symptomatic' levels for
#' `format_outcome_data_frame`.
#'
#' @keywords internal
format_extract_data_frame <- function(
  df,
  name,
  valid_levels,
  level_converter = identity,
  case_insensitive_levels = TRUE
) {
  # Input validation
  stopifnot(is.data.frame(df))
  if (!setequal(class(df), "data.frame")) {
    df <- as.data.frame(df)
  }
  colname <- names(df)
  if (length(colname) != 1L) {
    stop("The ", name, " data.frame should only contain one column.")
  }
  level_length <- valid_levels |>
    vapply(length, NA_integer_, USE.NAMES = FALSE) |>
    unique()
  if (length(level_length) != 1L) {
    stop(
      "Was given inconsistent valid level lengths: ",
      toString(level_length),
      "."
    )
  }

  # Extract and format the levels in `df`
  if (is.factor(df[, colname])) {
    df_levels <- levels(df[, colname])
  } else {
    df_levels <- unique(df[, colname])
  }
  if (case_insensitive_levels) {
    df_levels <- unique(tolower(df_levels))
  }
  if (length(df_levels) > level_length) {
    stop(
      "The ",
      name,
      " data.frame should only contain ",
      level_length,
      " types of ",
      name,
      "."
    )
  }
  levels_are_valid <- valid_levels |>
    vapply(\(x) length(setdiff(df_levels, x)) == 0L, NA, USE.NAMES = FALSE) |>
    any()
  if (!levels_are_valid) {
    stop(
      "The labels found in ",
      name,
      " weren't valid. ",
      "Was expecting something like ",
      toString(valid_levels),
      "."
    )
  }

  # Transform levels
  case_converter <- \(.) {
    if (case_insensitive_levels) tolower(.) else identity(.)
  }
  df[, colname] <- df[, colname] |>
    case_converter() |>
    level_converter() |>
    as.factor()

  # Done
  df
}


#' @rdname format_extract_data_frame
#' @keywords internal
format_surveillance_data_frame <- function(df) {
  format_extract_data_frame(
    df,
    "surveillance",
    list(c("active", "passive", "unknown"), c("a", "p", "u")),
    level_converter = function(x) {
      y <- substr(x, 1, 1)
      ifelse(y == "u", "Unknown", ifelse(y == "a", "Active", "Passive"))
    }
  )
}


#' @rdname format_extract_data_frame
#' @keywords internal
format_outcome_data_frame <- function(df) {
  format_extract_data_frame(
    df,
    "outcome",
    list(c("asymptomatic", "death", "symptomatic"), c("a", "d", "s")),
    level_converter = function(x) {
      first_letter <- substr(x, 1, 1)
      ifelse(
        first_letter == "a",
        "Asymptomatic",
        ifelse(
          first_letter == "d",
          "Death",
          "Symptomatic"
        )
      )
    }
  )
}
