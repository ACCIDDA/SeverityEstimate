#' @title
#' Extract And Match A Subset Of A data.frame
#'
#' @description
#' Extract a data.frame that is a unique subset of the given data.frame for the
#' columns and get the index of those rows in the subset.
#'
#' @param x A data.frame to extract and match from.
#' @param cols A character of columns to extract and match.
#' @param subset_x An optional data.frame that is a subset of `x` to use for
#' extracting. If `NULL` then is computed from `x`.
#' @param stop_on_nomatch A single logical indicating if an error should be
#' thrown in the event of a no match. This can only happen when `subset_x` is
#' given and does not cover the values in `x`.
#'
#' @return
#' A list with two named elements, the first is 'df' is the data.frame of the
#' extracted subset and 'ind' is the match index of `x` in 'df'.
#'
#' @keywords internal
extract_and_match_data_frame <- function(
  x,
  cols,
  subset_x = NULL,
  stop_on_nomatch = TRUE
) {
  if (is.null(subset_x)) {
    extract_x <- unique(x[, cols, drop = FALSE])
  } else {
    extract_x <- unique(subset_x[, cols, drop = FALSE])
  }
  if (length(cols)) {
    extract_x <- extract_x[
      do.call(order, extract_x[, cols, drop = FALSE]), , drop = FALSE
    ]
  }
  rownames(extract_x) <- seq_len(nrow(extract_x))
  if (length(extract_x)) {
    ind <- match(interaction(x[, cols]), interaction(extract_x))
  } else {
    ind <- rep.int(1L, nrow(x))
  }
  if (stop_on_nomatch && anyNA(ind)) {
    stop("There were values found in `x` not covered by `subset_x`.")
  }
  list("df" = extract_x, "ind" = ind)
}
