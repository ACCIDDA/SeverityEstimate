#' @title
#' Fit a severity estimate model instance.
#'
#' @param model A \linkS4class{SeverityEstimateModel} to fit.
#' @param ... Further optional args that are eventually given to [rstan::stan()]
#' related to fitting.
#'
#' @returns
#' A \linkS4class{SeverityEstimateFit} S4 object.
#'
#' @importFrom methods new
#' @importFrom utils tail
#' @export
fit <- function(model, ...) {
  # Validate required slots via pipeable require_* API
  ts <- model |>
    require_timesteps() |>
    timesteps()
  det <- model |>
    require_detection() |>
    detection()
  out <- model |>
    require_outcome() |>
    outcome()
  strata_l <- strata(model)

  # Ordered strata are not yet supported
  ordered_names <- Filter(
    Negate(is.null),
    sapply(strata_l, \(s) if (isTRUE(s$ordered)) s$name else NULL)
  )
  if (length(ordered_names) > 0L) {
    stop(
      "Ordered strata are not yet supported: ",
      toString(ordered_names),
      ". Please use `ordered = FALSE`.",
      call. = FALSE
    )
  }

  # Constants
  hazard_std <- 3.0

  # When no strata are configured, inject a synthetic single-level strata
  # column so incidence_population_arrays can work uniformly.
  synthetic_strata_col <- ".strata"
  no_strata <- length(strata_l) == 0L
  if (no_strata) {
    linelist <- model@line_list
    population <- model@population
    linelist[[synthetic_strata_col]] <- 1L
    population[[synthetic_strata_col]] <- 1L
    strata_cols <- synthetic_strata_col
    strata_ref_df <- data.frame(.strata = 1L)
    population_val <- utils::tail(names(model@population), n = 1L)
  } else {
    linelist <- model@line_list
    population <- model@population
    strata_cols <- sapply(strata_l, \(s) s$name)
    strata_ref_df <- do.call(
      expand.grid,
      c(
        lapply(strata_l, \(s) s$levels),
        list(stringsAsFactors = FALSE)
      )
    )
    names(strata_ref_df) <- strata_cols
    population_val <- utils::tail(names(model@population), n = 1L)
  }

  # Build reference data frames from slot levels so array dimensions respect
  # the user-specified ordering
  time_period_reference <- data.frame(
    x = ts$levels,
    stringsAsFactors = FALSE
  )
  names(time_period_reference) <- ts$name

  surveillance_reference <- data.frame(
    x = names(det$map),
    stringsAsFactors = FALSE
  )
  names(surveillance_reference) <- det$name

  outcome_reference <- data.frame(
    x = names(out$map),
    stringsAsFactors = FALSE
  )
  names(outcome_reference) <- out$name

  # Build incidence and population arrays
  arrays <- incidence_population_arrays(
    linelist,
    population,
    ts$name,
    strata_cols,
    det$name,
    out$name,
    population_val,
    time_period_reference,
    strata_ref_df,
    surveillance_reference,
    outcome_reference
  )

  # Drop the synthetic strata column from arrays$strata when no real strata
  if (no_strata) {
    arrays$strata <- arrays$strata[, character(0L), drop = FALSE]
  }

  # Format surveillance/outcome data frames to canonical factor levels
  surveillance_df <- format_surveillance_data_frame(arrays$surveillance)
  outcome_df <- format_outcome_data_frame(arrays$outcome)

  # Find active/passive positions in the formatted surveillance data frame.
  # format_surveillance_data_frame maps any value starting with "a" -> "Active"
  # and "p" -> "Passive", so we match the canonical formatted strings directly.
  surveillance_ind <- match(
    c("Active", "Passive"),
    as.character(surveillance_df[, det$name])
  )

  # Find symptomatic/death positions in the formatted outcome data frame.
  # format_outcome_data_frame canonicalises to "Asymptomatic"/"Symptomatic"/"Death".
  # symptoms_* is TRUE for both symptomatic and death (presented with symptoms);
  # dead_* is TRUE only for death.
  outcome_ind <- match(
    c("Asymptomatic", "Symptomatic", "Death"),
    as.character(outcome_df[, out$name])
  )
  # outcome_ind[2:3] = symptomatic + death indices; outcome_ind[3] = death only
  outcome_symp_ind <- outcome_ind[2L:3L]
  outcome_death_ind <- outcome_ind[3L]

  # Collapse surveillance dimension for I_active / I_passive
  incidence_without_outcome <- rowSums(arrays$incidence, dims = 3L)
  active_ind_ll <- which(
    arrays$linelist_ind[, "surveillance"] == surveillance_ind[1L]
  )
  passive_ind_ll <- which(
    arrays$linelist_ind[, "surveillance"] == surveillance_ind[2L]
  )

  # Build the strata_index matrix for the template (strata_groups x n_dims).
  # Each row is a strata cell; each column is the per-dimension level index.
  # Also build template_strata describing each dimension for the Jinja2 template.
  if (length(strata_l) > 0L) {
    n_strata_dims <- length(strata_l)
    strata_index <- matrix(
      0L,
      nrow = nrow(arrays$strata),
      ncol = n_strata_dims
    )
    template_strata <- vector("list", n_strata_dims)
    for (k in seq_len(n_strata_dims)) {
      s <- strata_l[[k]]
      col_vals <- arrays$strata[, s$name]
      strata_index[, k] <- match(col_vals, s$levels)
      template_strata[[k]] <- list(
        name = s$name,
        n_levels = length(s$levels)
      )
    }
  } else {
    strata_index <- NULL
    template_strata <- list()
  }

  # Extract prior parameterizations
  active_prior <- active_prior(model)
  passive_asym <- passive_asymptomatic_prior(model)
  passive_sym <- passive_symptomatic_prior(model)

  # Compile Stan data list
  data <- list(
    strata_groups = nrow(arrays$strata),
    time_groups = nrow(arrays$time_period),
    I_active = matrix(
      incidence_without_outcome[,, surveillance_ind[1L]],
      nrow = nrow(arrays$time_period),
      ncol = nrow(arrays$strata)
    ),
    I_passive = matrix(
      incidence_without_outcome[,, surveillance_ind[2L]],
      nrow = nrow(arrays$time_period),
      ncol = nrow(arrays$strata)
    ),
    population = arrays$population,
    observed_active = length(active_ind_ll),
    observed_passive = length(passive_ind_ll),
    strata_active = as_integer_array(
      arrays$linelist_ind[active_ind_ll, "strata", drop = TRUE]
    ),
    symptoms_active = as_integer_array(
      arrays$linelist_ind[active_ind_ll, "outcome"] %in% outcome_symp_ind
    ),
    dead_active = as_integer_array(
      arrays$linelist_ind[active_ind_ll, "outcome"] %in% outcome_death_ind
    ),
    strata_passive = as_integer_array(
      arrays$linelist_ind[passive_ind_ll, "strata", drop = TRUE]
    ),
    symptoms_passive = as_integer_array(
      arrays$linelist_ind[passive_ind_ll, "outcome"] %in% outcome_symp_ind
    ),
    dead_passive = as_integer_array(
      arrays$linelist_ind[passive_ind_ll, "outcome"] %in% outcome_death_ind
    ),
    hazard_std = hazard_std,
    active_detection_alpha = active_prior[["alpha"]],
    active_detection_beta = active_prior[["beta"]],
    passive_asymptomatic_alpha = passive_asym[["alpha"]],
    passive_asymptomatic_beta = passive_asym[["beta"]],
    passive_symptomatic_alpha = passive_sym[["alpha"]],
    passive_symptomatic_beta = passive_sym[["beta"]]
  )
  if (!is.null(strata_index)) {
    data[["strata_index"]] <- strata_index
  }

  # Fit via Jinja2 template
  model_fit <- stan_model(
    "estimate_severity.stan.j2",
    template_data = list(
      strata = template_strata,
      n_strata_dims = length(template_strata)
    ),
    data = data,
    ...
  )

  # Return SeverityEstimateFit
  new(
    "SeverityEstimateFit",
    model_fit = model_fit,
    population = arrays$population,
    incidence = arrays$incidence,
    time_period = arrays$time_period,
    strata = arrays$strata,
    surveillance = surveillance_df,
    outcome = outcome_df
  )
}
