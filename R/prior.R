#' @title
#' Internal Prior Setter Helper
#'
#' @description
#' Internal helper used by replacement methods to validate and set prior
#' parameterizations on a \linkS4class{SeverityEstimateModel}.
#'
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param parameter The model parameter to specify the prior for, should be one
#' of `active`, `passive_asymptomatic`, `passive_symptomatic`.
#' @param ... Beta distribution parameterization. Must be one of `alpha`/`beta`,
#' `mean`/`var`, `mean`/`sd`.
#'
#' @return
#' `model` modified to contain the prior parameterization for `parameter`.
#'
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_list
#' @importFrom methods slot<-
#' @rdname prior-internal
#' @keywords internal
prior <- function(model, parameter, ...) {
  # Check the prior parameter name
  checkmate::assert_choice(
    parameter,
    c("active", "passive_asymptomatic", "passive_symptomatic")
  )
  parameter_prior <- paste0(parameter, "_prior")
  # Check the parameterization given
  args <- list(...)
  checkmate::assert_list(
    args,
    types = "numeric",
    any.missing = FALSE,
    len = 2L,
    names = "strict"
  )
  storage.mode(args) <- "double"
  # Check the model
  check_model(model, attribute = parameter_prior)
  # Assign parameterization to the model
  methods::slot(model, parameter_prior) <- beta_parameterization(args)
  model
}

#' @title
#' Internal Prior Getter Helper
#'
#' @description
#' Internal helper used by getter methods to extract configured priors from a
#' \linkS4class{SeverityEstimateModel}, returning a default prior with warning
#' if unset.
#'
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param parameter The model parameter to get the prior for, should be one
#' of `active`, `passive_asymptomatic`, `passive_symptomatic`.
#'
#' @return
#' A named numeric vector of `alpha` and `beta`.
#'
#' @importFrom checkmate assert_choice
#' @importFrom methods slot
#' @rdname prior-internal
#' @keywords internal
get_prior <- function(model, parameter) {
  checkmate::assert_choice(
    parameter,
    c("active", "passive_asymptomatic", "passive_symptomatic")
  )
  parameter_prior <- paste0(parameter, "_prior")
  prior <- methods::slot(model, parameter_prior)
  if (!length(prior)) {
    warning(
      "The given 'model' has no ",
      gsub("_", " ", parameter, fixed = TRUE),
      " prior set. Returning the default uniformative prior alpha = 1, beta = 1."
    )
    return(c(alpha = 1.0, beta = 1.0))
  }
  prior
}

#' @title
#' Get Or Set Model Prior Parameterizations
#'
#' @description
#' S4 getter and replacement methods for prior parameterizations on a
#' \linkS4class{SeverityEstimateModel}, plus chainable `set_*` helpers for
#' pipeline ergonomics.
#'
#' @param x A \linkS4class{SeverityEstimateModel}.
#' @param value A named list or named numeric vector providing a beta
#' parameterization. Must be one of `alpha`/`beta`, `mean`/`var`, `mean`/`sd`.
#'
#' @return
#' `active_prior(x)` returns the current active prior parameterization. If the
#' active prior has not yet been set, a warning is issued and the default prior
#' `c(alpha = 1.0, beta = 1.0)` is returned.
#'
#' `active_prior(x) <- value` returns `x` modified to contain the active prior
#' parameterization.
#'
#' `set_active_prior(model, ...)` returns `model` modified to contain the active
#' prior parameterization.
#'
#' `passive_asymptomatic_prior(x)` returns the current passive asymptomatic
#' prior parameterization. If the passive asymptomatic prior has not yet been
#' set, a warning is issued and the default prior `c(alpha = 1.0, beta = 1.0)`
#' is returned.
#'
#' `passive_asymptomatic_prior(x) <- value` returns `x` modified to contain the
#' passive asymptomatic prior parameterization.
#'
#' `set_passive_asymptomatic_prior(model, ...)` returns `model` modified to
#' contain the passive asymptomatic prior parameterization.
#'
#' `passive_symptomatic_prior(x)` returns the current passive symptomatic prior
#' parameterization. If the passive symptomatic prior has not yet been set, a
#' warning is issued and the default prior `c(alpha = 1.0, beta = 1.0)` is
#' returned.
#'
#' `passive_symptomatic_prior(x) <- value` returns `x` modified to contain the
#' passive symptomatic prior parameterization.
#'
#' `set_passive_symptomatic_prior(model, ...)` returns `model` modified to
#' contain the passive symptomatic prior parameterization.
#'
#' @examples
#' line_list <- data.frame(
#'   patient = 1L:3L,
#'   week = c(1L, 1L, 2L),
#'   age = c("Youth", "Adult", "Senior"),
#'   detection = c("Active", "Passive", "Active"),
#'   outcome = c("Asymptomatic", "Death", "Symptomatic")
#' )
#' population <- data.frame(
#'   age = c("Youth", "Adult", "Senior"),
#'   amount = rep(987L, 3L)
#' )
#' model <- SeverityEstimateModel(line_list, population) |>
#'   set_active_prior(mean = 0.9, sd = 0.08) |>
#'   set_passive_asymptomatic_prior(alpha = 1.0, beta = 1.0) |>
#'   set_passive_symptomatic_prior(mean = 0.1, var = 0.0064)
#' model
#'
#' @importFrom methods setGeneric
#' @rdname active_prior
#' @export
methods::setGeneric(
  "active_prior",
  function(x) standardGeneric("active_prior")
)

#' @importFrom methods setGeneric
#' @rdname active_prior
#' @export
methods::setGeneric(
  "active_prior<-",
  function(x, value) standardGeneric("active_prior<-")
)

#' @importFrom methods setMethod
#' @importFrom methods slot
#' @rdname active_prior
#' @export
methods::setMethod(
  "active_prior",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    get_prior(x, "active")
  }
)

#' @rdname active_prior
#' @export
methods::setMethod(
  "active_prior<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (is.atomic(value) && !is.null(names(value))) {
      value <- as.list(value)
    }
    do.call(
      prior,
      append(list(model = x, parameter = "active"), value)
    )
  }
)

#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param ... Beta distribution parameterization. Must be one of `alpha`/`beta`,
#' `mean`/`var`, `mean`/`sd`.
#'
#' @rdname active_prior
#' @export
set_active_prior <- function(model, ...) {
  active_prior(model) <- list(...)
  model
}

#' @rdname active_prior
#' @export
methods::setGeneric(
  "passive_asymptomatic_prior",
  function(x) standardGeneric("passive_asymptomatic_prior")
)

#' @rdname active_prior
#' @export
methods::setGeneric(
  "passive_asymptomatic_prior<-",
  function(x, value) standardGeneric("passive_asymptomatic_prior<-")
)

#' @rdname active_prior
#' @export
methods::setMethod(
  "passive_asymptomatic_prior",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    get_prior(x, "passive_asymptomatic")
  }
)

#' @rdname active_prior
#' @export
methods::setMethod(
  "passive_asymptomatic_prior<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (is.atomic(value) && !is.null(names(value))) {
      value <- as.list(value)
    }
    do.call(
      prior,
      append(list(model = x, parameter = "passive_asymptomatic"), value)
    )
  }
)

#' @rdname active_prior
#' @export
set_passive_asymptomatic_prior <- function(model, ...) {
  passive_asymptomatic_prior(model) <- list(...)
  model
}

#' @rdname active_prior
#' @export
methods::setGeneric(
  "passive_symptomatic_prior",
  function(x) standardGeneric("passive_symptomatic_prior")
)

#' @rdname active_prior
#' @export
methods::setGeneric(
  "passive_symptomatic_prior<-",
  function(x, value) standardGeneric("passive_symptomatic_prior<-")
)

#' @rdname active_prior
#' @export
methods::setMethod(
  "passive_symptomatic_prior",
  signature(x = "SeverityEstimateModel"),
  function(x) {
    get_prior(x, "passive_symptomatic")
  }
)

#' @rdname active_prior
#' @export
methods::setMethod(
  "passive_symptomatic_prior<-",
  signature(x = "SeverityEstimateModel"),
  function(x, value) {
    if (is.atomic(value) && !is.null(names(value))) {
      value <- as.list(value)
    }
    do.call(
      prior,
      append(list(model = x, parameter = "passive_symptomatic"), value)
    )
  }
)

#' @rdname active_prior
#' @export
set_passive_symptomatic_prior <- function(model, ...) {
  passive_symptomatic_prior(model) <- list(...)
  model
}
