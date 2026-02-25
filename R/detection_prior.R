#' @title
#' Helper to extract a detection prior from a severity estimate model.
#'
#' @param name The name to display in the warning describing the type of
#' detection rate being extracted, e.g. 'active' or 'passive symptomatic'.
#' @param prior A beta parameterized numeric vector for the prior distribution.
#' @param default A default beta parameterization to use, or if `NULL` then the
#' uniformative prior of `c("alpha" = 1.0, "beta" = 2.0)` is used.
#'
#' @returns
#' A numeric vector containing containing the parameterization for a beta prior.
#'
#' @keywords internal
detection_prior <- function(name, prior, default = NULL) {
  if (length(prior) > 0L) {
    return(prior)
  }
  if (is.null(default)) {
    default <- c("alpha" = 1.0, "beta" = 2.0)
  }
  warning(
    "A ",
    name,
    " detection prior has not been set, ",
    "defaulting to the uniformative prior of ",
    pretty_named_vector(default),
    "."
  )
  default
}
