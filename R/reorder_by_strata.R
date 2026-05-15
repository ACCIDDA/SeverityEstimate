#' @title
#' Reorder A `data.frame` Using Strata Reference Rows
#'
#' @description
#' Reorder a `data.frame` so the strata columns follow the row order of a
#' reference strata `data.frame`.
#'
#' @param x A `data.frame` containing the strata columns to reorder by.
#' @param strata A reference `data.frame` whose rows define the desired strata
#' order.
#'
#' @return
#' A reordered `data.frame` with row names reset.
#'
#' @keywords internal
reorder_by_strata <- function(x, strata) {
  stopifnot(is.data.frame(x), is.data.frame(strata))
  if (!setequal(class(x), "data.frame")) {
    x <- as.data.frame(x)
  }
  if (!setequal(class(strata), "data.frame")) {
    strata <- as.data.frame(strata)
  }

  strata_cols <- names(strata)
  if (length(strata_cols) == 0L || nrow(x) <= 1L) {
    rownames(x) <- NULL
    return(x)
  }
  if (!all(strata_cols %in% names(x))) {
    stop(
      "The given `x` is missing strata columns: ",
      toString(setdiff(strata_cols, names(x))),
      "."
    )
  }

  x_strata <- x[, strata_cols, drop = FALSE]
  strata_idx <- match(
    interaction(x_strata, drop = TRUE, lex.order = TRUE),
    interaction(strata, drop = TRUE, lex.order = TRUE)
  )
  if (anyNA(strata_idx)) {
    stop("The given `x` contains strata rows not covered by `strata`.")
  }

  x <- x[order(strata_idx), , drop = FALSE]
  rownames(x) <- NULL
  x
}
