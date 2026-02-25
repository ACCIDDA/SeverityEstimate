#' @keywords internal
pretty_named_vector <- function(x) {
  paste(names(x), x, sep = "=", collapse = ", ")
}
