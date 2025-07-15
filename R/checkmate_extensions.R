#' @title
#' Checkmate Extensions
#'
#' @description
#' This is a small wrapper around [checkmate::check_number()] specifically for
#' probabilities for convenience.
#'
#' Convenience wrappers around checkmate for specific internal use cases.
#' Namely:
#' * `check_probability`/`assert_probability` are wrappers around
#' [checkmate::check_number()] specifically for probabilities, and
#' * `assert_bool` is a wrappeer around [checkmate::check_false()] and
#' [checkmate::check_true()] for single length logicals.
#'
#' @inheritParams checkmate::check_number
#' @inheritParams checkmate::check_false
#' @inheritParams chekcamte::check_true
#'
#' @importFrom checkmate assert
#' @importFrom checkmate check_false
#' @importFrom checkmate check_number
#' @importFrom checkmate check_true
#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
#' @rdname checkmate_extensions
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

#' @rdname checkmate_extensions
assert_probability <- checkmate::makeAssertionFunction(check_probability)

#' @rdname checkmate_extensions
assert_bool <- function(x, na.ok = FALSE, .var.name = checkmate::vname(x)) {
  checkmate::assert(
    checkmate::check_true(x, na.ok = na.ok),
    checkmate::check_false(x, na.ok = na.ok),
    combine = "or",
    .var.name = .var.name
  )
}
