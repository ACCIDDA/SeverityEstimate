#' @title
#' Is `x` A 'data.frame'?
#'
#' @description
#' Checks if the given variable is a 'data.frame' like object and if it is not
#' exactly a 'data.frame' will convert it to one.
#'
#' @param x The object to check and downcast.
#'
#' @return
#' Either `x` if it is only a 'data.frame' otherwise `x` downcasted as a
#' 'data.frame'.
#'
#' @importFrom checkmate vname
#' @keywords internal
is_data_frame <- function(x) {
  if (!is.data.frame(x)) {
    stop("`", checkmate::vname(x), "` is not 'data.frame' like.")
  }
  if (!setequal(class(x), "data.frame")) {
    x <- as.data.frame(x)
  }
  x
}
