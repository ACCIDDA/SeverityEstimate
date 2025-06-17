#' @title
#' Process A Vector Or `data.frame` Of Reference Values
#'
#' @description
#' Convert a given set of values into a formatted `data.frame` for reference.
#'
#' @param values Either `NULL`, a `data.frame`, or a vector to convert into a
#' `data.frame`.
#' @param columns The expected columns of the output reference `data.frame`. If
#' `values` is a vector then this can only be a single length vector.
#'
#' @return
#' If `values` is `NULL` then `NULL` otherwise a `data.frame` with the columns
#' specified by `columns`.
#'
#' @keywords internal
process_reference <- function(values, columns) {
  if (is.null(values)) {
    return(NULL)
  }
  if (is.data.frame(values)) {
    if (!setequal(class(values), "data.frame")) {
      values <- as.data.frame(values)
    }
    missing_columns <- setdiff(columns, names(values))
    if (length(missing_columns) != 0L) {
      stop(
        "The values given is a data.frame but is missing expected columns: ",
        toString(missing_columns),
        "."
      )
    }
    values <- values[, columns, drop = FALSE]
    return(values)
  }
  length_columns <- length(columns)
  if (length_columns != 1L) {
    stop(
      "If a non-data.frame is provided for values only one column can be ",
      "given, but instead was given ",
      length_columns,
      " columns."
    )
  }
  values <- data.frame(values)
  names(values) <- columns
  values
}
