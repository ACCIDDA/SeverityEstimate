#' @title Build Strata Basis Terms And Design Matrices
#'
#' @description
#' Helpers for turning ordered or categorical strata specifications into basis
#' terms for the `fit()` Stan model. `build_strata_level_basis()` constructs the
#' per-dimension basis matrix for one strata variable, while
#' `build_strata_design_matrix()` expands those terms across the full strata-cell
#' grid used by the model.
#'
#' @param strata A list of strata specifications from `strata(model)`.
#' @param strata_df The expanded strata-cell data frame returned by
#' [incidence_population_arrays()].
#' @param levels The ordered levels for the dimension.
#' @param degrees_of_freedom The requested degrees of freedom.
#'
#' @return
#' `build_strata_design_matrix()` returns a named list with `X_strata`, the
#' additive design matrix, and `n_strata_basis_cols`, its column count.
#'
#' `build_strata_level_basis()` returns a numeric matrix with one row per level
#' and one column per basis term.
#'
#' @examples
#' strata_spec <- list(
#'   list(
#'     name = "age",
#'     levels = c("Youth", "Adult", "Senior"),
#'     degrees_of_freedom = 1L
#'   ),
#'   list(
#'     name = "region",
#'     levels = c("North", "South"),
#'     degrees_of_freedom = 0L
#'   )
#' )
#' strata_df <- expand.grid(
#'   age = c("Youth", "Adult", "Senior"),
#'   region = c("North", "South"),
#'   stringsAsFactors = FALSE
#' )
#' # jarl-ignore internal_function: example
#' SeverityEstimate:::build_strata_design_matrix(strata_spec, strata_df)
#'
#' @importFrom checkmate assert_data_frame
#' @importFrom checkmate assert_list
#' @keywords internal
build_strata_design_matrix <- function(strata, strata_df) {
  checkmate::assert_list(strata)
  checkmate::assert_data_frame(strata_df, min.rows = 1L)

  if (length(strata) == 0L) {
    return(list(
      X_strata = matrix(numeric(), nrow = nrow(strata_df), ncol = 0L),
      n_strata_basis_cols = 0L
    ))
  }

  x_parts <- vector("list", length(strata))
  n_strata_basis_cols <- 0L

  for (k in seq_along(strata)) {
    spec <- strata[[k]]
    basis <- build_strata_level_basis(spec$levels, spec$degrees_of_freedom)
    level_ind <- match(strata_df[, spec$name, drop = TRUE], spec$levels)
    if (anyNA(level_ind)) {
      stop(
        "Internal error: Failed to match strata levels for '",
        spec$name,
        "'.",
        call. = FALSE
      )
    }

    if (ncol(basis) == 0L) {
      x_parts[[k]] <- matrix(numeric(), nrow = nrow(strata_df), ncol = 0L)
    } else {
      x_parts[[k]] <- basis[level_ind, , drop = FALSE]
      colnames(x_parts[[k]]) <- paste0(spec$name, "_", seq_len(ncol(basis)))
    }
    n_strata_basis_cols <- n_strata_basis_cols + ncol(basis)
  }

  if (n_strata_basis_cols == 0L) {
    x_strata <- matrix(numeric(), nrow = nrow(strata_df), ncol = 0L)
  } else {
    x_strata <- do.call(cbind, x_parts)
  }
  storage.mode(x_strata) <- "double"

  list(
    X_strata = x_strata,
    n_strata_basis_cols = n_strata_basis_cols
  )
}

#' @rdname build_strata_design_matrix
#' @examples
#' # jarl-ignore internal_function: example
#' SeverityEstimate:::build_strata_level_basis(
#'   c("Youth", "Adult", "Senior"),
#'   degrees_of_freedom = 0L
#' )
#' # jarl-ignore internal_function: example
#' SeverityEstimate:::build_strata_level_basis(
#'   c("Youth", "Adult", "Senior", "Elderly"),
#'   degrees_of_freedom = 2L
#' )
#'
#' @importFrom checkmate assert_integerish
#' @importFrom stats contr.sum
#' @importFrom stats poly
build_strata_level_basis <- function(levels, degrees_of_freedom) {
  checkmate::assert_integerish(degrees_of_freedom, len = 1L, lower = 0L)
  degrees_of_freedom <- as.integer(degrees_of_freedom)
  n_levels <- length(levels)

  if (n_levels == 0L) {
    stop("Internal error: `levels` must have positive length.", call. = FALSE)
  }

  if (degrees_of_freedom == 0L) {
    if (n_levels == 1L) {
      basis <- matrix(numeric(), nrow = 1L, ncol = 0L)
    } else {
      basis <- stats::contr.sum(n_levels)
    }
  } else {
    basis <- stats::poly(
      seq_len(n_levels),
      degree = degrees_of_freedom,
      simple = TRUE
    )
  }

  basis <- as.matrix(basis)
  if (ncol(basis) == 0L) {
    rownames(basis) <- as.character(levels)
    return(basis)
  }

  basis <- unclass(scale(basis, center = TRUE, scale = TRUE))
  rownames(basis) <- as.character(levels)
  storage.mode(basis) <- "double"
  basis
}
