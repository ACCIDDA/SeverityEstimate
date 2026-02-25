#' @title
#' Coerce Vectors To One-Dimensional Arrays
#'
#' @description
#' Internal helpers for coercing vectors to one-dimensional arrays, with an
#' optional conversion step before array construction.
#'
#' @param x A vector-like object to convert to an array.
#' @param converter A function applied to `x` before constructing the array.
#'
#' @return
#' A one-dimensional array with length equal to `length(x)`.
#'
#' @keywords internal
as_array <- function(x, converter = \(x) x) {
  array(converter(x), dim = length(x))
}

#' @rdname as_array
#' @keywords internal
as_integer_array <- function(x) {
  as_array(x, converter = as.integer)
}

#' @rdname as_array
#' @keywords internal
as_numeric_array <- function(x) {
  as_array(x, converter = as.numeric)
}
