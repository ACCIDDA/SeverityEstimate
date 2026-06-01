#' @name summary_estimate_model_helpers
#' @title
#' Internal Helpers For `SummaryEstimateModel`
#'
#' @description
#' Internal helpers used to construct and format `SummaryEstimateModel` objects.
#'
#' @param data A `data.frame` summarising model input data dimensions.
#' @param priors A `data.frame` summarising beta priors.
#' @param timesteps A `data.frame` summarising the timestep specification.
#' @param detection A `data.frame` summarising the detection mapping and counts.
#' @param outcome A `data.frame` summarising the outcome mapping and counts.
#' @param strata A `data.frame` summarising strata specifications.
#' @param model A \linkS4class{SeverityEstimateModel}.
#' @param specification A model mapping specification.
#' @param valid_types Canonical model types represented in a mapping.
#' @param x A numeric vector to format or mapping summary `data.frame` to
#' print.
#' @param digits The number of significant digits to keep.
#'
#' @return
#' `new_summary_estimate_model` returns an object of class
#' `SummaryEstimateModel`.
#'
#' @keywords internal
NULL


#' @rdname summary_estimate_model_helpers
new_summary_estimate_model <- function(
  data,
  priors,
  timesteps,
  detection,
  outcome,
  strata
) {
  structure(
    list(
      data = data,
      priors = priors,
      timesteps = timesteps,
      detection = detection,
      outcome = outcome,
      strata = strata
    ),
    class = "SummaryEstimateModel"
  )
}


#' @rdname summary_estimate_model_helpers
format_summary_model_data <- function(model) {
  data.frame(
    dataset = c("line_list", "population"),
    rows = c(nrow(model@line_list), nrow(model@population)),
    columns = c(ncol(model@line_list), ncol(model@population)),
    check.names = FALSE
  )
}


#' @rdname summary_estimate_model_helpers
format_summary_model_priors <- function(model) {
  prior_parameters <- c(
    "active",
    "passive_asymptomatic",
    "passive_symptomatic"
  )
  prior_slots <- paste0(prior_parameters, "_prior")
  priors <- lapply(prior_slots, function(prior_slot) {
    prior <- methods::slot(model, prior_slot)
    default <- !length(prior)
    if (default) {
      prior <- c(alpha = 1.0, beta = 1.0)
    }
    data.frame(
      alpha = unname(prior[["alpha"]]),
      beta = unname(prior[["beta"]]),
      default = default,
      check.names = FALSE
    )
  })
  priors <- do.call(rbind, priors)
  priors <- cbind(
    data.frame(parameter = prior_parameters, check.names = FALSE),
    priors
  )
  row.names(priors) <- NULL
  priors
}


#' @rdname summary_estimate_model_helpers
format_summary_model_timesteps <- function(model) {
  if (!has_timesteps(model)) {
    return(data.frame())
  }
  levels <- model@timesteps$levels
  data.frame(
    column = model@timesteps$name,
    start = if (length(levels)) as.character(levels[[1L]]) else NA_character_,
    end = if (length(levels)) {
      as.character(levels[[length(levels)]])
    } else {
      NA_character_
    },
    timesteps = length(levels),
    check.names = FALSE
  )
}


#' @rdname summary_estimate_model_helpers
format_summary_model_detection <- function(model) {
  format_summary_model_map(
    model = model,
    specification = model@detection,
    valid_types = c("active", "passive")
  )
}


#' @rdname summary_estimate_model_helpers
format_summary_model_outcome <- function(model) {
  format_summary_model_map(
    model = model,
    specification = model@outcome,
    valid_types = c("asymptomatic", "symptomatic", "severe")
  )
}


#' @rdname summary_estimate_model_helpers
format_summary_model_map <- function(model, specification, valid_types) {
  if (!length(specification)) {
    return(data.frame())
  }

  values <- as.character(model@line_list[, specification$name, drop = TRUE])
  mapped_values <- unname(specification$map[values])
  counts <- table(factor(mapped_values, levels = valid_types))

  mapping_summary <- data.frame(
    column = specification$name,
    type = valid_types,
    values = vapply(
      valid_types,
      function(valid_type) {
        raw_values <- names(specification$map)[specification$map == valid_type]
        toString(raw_values)
      },
      character(1L)
    ),
    cases = as.integer(counts[valid_types]),
    check.names = FALSE
  )
  row.names(mapping_summary) <- NULL
  mapping_summary
}


#' @rdname summary_estimate_model_helpers
format_summary_model_strata <- function(model) {
  if (!length(model@strata)) {
    return(data.frame())
  }

  do.call(
    rbind,
    lapply(model@strata, function(strata_specification) {
      data.frame(
        column = strata_specification$name,
        degrees_of_freedom = strata_specification$degrees_of_freedom,
        levels = toString(strata_specification$levels),
        n_levels = length(strata_specification$levels),
        check.names = FALSE
      )
    })
  )
}


#' @rdname summary_estimate_model_helpers
format_summary_model_number <- function(x, digits = 3L) {
  formatted <- format(signif(x, digits), trim = TRUE, scientific = FALSE)
  has_decimal <- grepl(".", formatted, fixed = TRUE)
  formatted[!has_decimal] <- paste0(formatted[!has_decimal], ".0")
  formatted
}


#' @rdname summary_estimate_model_helpers
print_summary_model_map <- function(x) {
  if (!nrow(x)) {
    cat("  not set\n")
    return(invisible(x))
  }
  cat("  column: ", unique(x$column), "\n", sep = "")
  for (idx in seq_len(nrow(x))) {
    row <- x[idx, , drop = FALSE]
    cat(
      "    ",
      row$type,
      ": ",
      row$cases,
      " cases",
      " (values: ",
      row$values,
      ")\n",
      sep = ""
    )
  }
  invisible(x)
}
