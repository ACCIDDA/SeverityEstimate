#' @title
#' Create An Array From Vectors Of Indices And Values
#'
#' @description
#' Create an array from vectors of indices, such as (i,j,k) notation, with
#' optional values and dimnames for the output array.
#'
#' @param ... Indices given either as an arbitrary number of equal length
#' numerics or as a single list containing an arbitrary number of equal length
#' numerics.
#' @param target The value to populate the array values with. This parameter
#' will be coerced to match the length of the indices given.
#' @param dim The dim attribute for the array to be created (like
#' [base::array()]) or if `NULL` will be inferred from the max index value along
#' each dimension provided for `...`. If not `NULL` then the length must match
#' that of `...`.
#' @param dimnames Either a named list with corresponding dimname vectors (like
#' [base::array()]), a character vector to just name the dimnames and use a
#' default sequence for the dimname values, or `NULL` for not assigning dimnames
#' to the output.
#'
#' @details
#' Overlapping indices will be summed. For example if this function was given
#' `c(1L, 1L)` and `c(1L, 1L)` for `...` then the resulting array would be 1 x 1
#' with the entry being `2L`.
#'
#' When given `NULL` or a list for `dimnames` that argument has the same
#' behavior as [base::array()]. However, this function also provides the ability
#' to just provide the dimname names as a character and populate the dimnames
#' values with a sequence (`1L:N`). This third option for the `dimnames`
#' argument is for situations where the output array dimensions is not known in
#' advance without taking the `max` of each index vector given.
#'
#' @return
#' A array with the same number of dimensions as given by `...` populated with
#' the values from `target`.
#'
#' @importFrom stats as.formula
#' @importFrom stats xtabs
#' @keywords internal
construct_array_from_indices <- function(
  ...,
  target = 1L,
  dim = NULL,
  dimnames = NULL
) {
  # Input validation
  stopifnot(
    is.numeric(target),
    is.null(dimnames) || is.character(dimnames) || is.list(dimnames)
  )

  # Process indices
  indices <- list(...)
  if (length(indices) == 0L) {
    stop("At least one vector of indices must be provided for '...'.")
  }
  if (is.list(indices[[1L]])) {
    if (length(indices) > 1L) {
      stop(
        "The first element of '...' is a list ",
        "but more than one argument was given."
      )
    }
    indices <- indices[[1L]]
  }
  if (!is.null(dim) && length(dim) != length(indices)) {
    stop(
      "An explicit `dim` was given, with length ", length(dim), ", which ",
      "does not match the number of indicies given, ", length(indices), "."
    )
  }

  # Light validation and input restructuring
  index_lengths <- vapply(indices, length, NA_integer_, USE.NAMES = FALSE)
  index_length <- unique(index_lengths)
  if (length(index_length) > 1L) {
    stop(
      "The indices given for '...' must all be equal length, instead ",
      "was given arguments with lengths: ", toString(index_length), "."
    )
  }
  if (length(target) != index_length) {
    target <- rep(target, index_length)
  }

  # NOTE: From this point on could be written in C for performance if needed.

  # Fill in missing indices
  for (i in seq_along(indices)) {
    if (is.null(dim)) {
      missing_ind <- setdiff(seq_len(max(indices[[i]])), indices[[i]])
    } else {
      missing_ind <- setdiff(seq_len(dim[i]), indices[[i]])
    }
    if (length(missing_ind) > 0L) {
      indices[[i]] <- append(indices[[i]], missing_ind)
      missing_ind <- length(missing_ind)
      target <- append(target, rep.int(0L, missing_ind))
      for (j in seq_along(indices)) {
        if (i == j) {
          next
        }
        indices[[j]] <- append(
          indices[[j]],
          rep.int(indices[[j]][1L], missing_ind)
        )
      }
    }
  }

  # Convert indices list to an env
  indices_names <- paste0("a", seq_along(indices))
  names(indices) <- indices_names
  indices[["target"]] <- target
  indices <- list2env(indices)

  # Build the array
  index_formula <- stats::as.formula(
    paste0("target ~ ", paste(indices_names, collapse = " + ")),
    env = indices
  )
  output_array <- index_formula |>
    stats::xtabs() |>
    unclass()
  attr(output_array, "call") <- NULL
  dimnames(output_array) <- list()
  if (!is.null(dimnames) && length(dimnames)) {
    if (is.character(dimnames)) {
      dimnames <- mapply(
        function(x, y) seq_len(y),
        dimnames,
        dim(output_array), SIMPLIFY = FALSE
      )
    }
    dimnames(output_array) <- dimnames
  }

  # Done
  output_array
}
