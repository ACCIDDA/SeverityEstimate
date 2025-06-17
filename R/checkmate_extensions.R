#' @title
#' Checkmate Extensions For Probabilities
#'
#' @description
#' This is a small wrapper around [checkmate::check_number()] specifically for
#' probabilities for convenience.
#'
#' @inheritParams checkmate::check_number
#'
#' @importFrom checkmate check_number
#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
check_probability <- function(x) {
  checkmate::check_number(
    x,
    na.ok = FALSE,
    lower = 0.0,
    upper = 1.0,
    finite = TRUE,
    null.ok = TRUE
  )
}

#' @rdname check_probability
assert_probability <- checkmate::makeAssertionFunction(check_probability)
