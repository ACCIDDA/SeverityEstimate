#' @title
#' Is `x` A 'data.frame'?
#'
#' @description
#' Checks if the given variable is a 'data.frame' like object and if it is not
#' exactly a 'data.frame' will convert it to one.
#'
#' @param x The object to check and downcast.
#' @param has_string_columns A character vector of columns to check for the
#' presence of as well as check it is a character/factor column.
#'
#' @return
#' Either `x` if it is only a 'data.frame' otherwise `x` downcasted as a
#' 'data.frame'.
#'
#' @importFrom checkmate vname
#' @keywords internal
is_data_frame <- function(x, has_string_columns = character()) {
  if (!is.data.frame(x)) {
    stop("`", checkmate::vname(x), "` is not 'data.frame' like.")
  }
  if (!setequal(class(x), "data.frame")) {
    x <- as.data.frame(x)
  }
  if (length(has_string_columns)) {
    missing_columns <- setdiff(has_string_columns, names(x))
    if (length(missing_columns)) {
      stop(
        "`",
        checkmate::vname(x),
        "` is missing required string columns: ",
        toString(missing_columns),
        "."
      )
    }
    for (column in has_string_columns) {
      y <- x[, column]
      if (!is.factor(y) && !is.character(y)) {
        stop(
          "The '",
          column,
          "' column of `",
          checkmate::vname(x),
          "` is not a character or factor, instead is: ",
          toString(class(y)),
          "."
        )
      }
    }
  }
  x
}
