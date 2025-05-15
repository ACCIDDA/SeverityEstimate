#' @title
#' Create A Sample Line List
#'
#' @description
#' Generates a sample line list that can be used as an example data set for
#' analysis with this package.
#'
#' @param strata A data.frame like object with the columns 'population', 'sir',
#' and 'ifr'. Any additional columns are treated as descriptions for the strata.
#' If `strata` only contains the columns 'population', 'sir', 'ifr' it's assumed
#' that the sample data set contains no strata.
#' @param times A vector denoting time steps for the sample data set.
#' Must be provided in order.
#' @param active_detection The probability of detecting a case through active
#' detection.
#' @param passive_asymptomatic_detection The probability of detecting a case
#' presenting asymptomatically through passive surveillance.
#' @param passive_symptomatic_detection The probability of detecting a case
#' presenting symptomatically through passive surveillance.
#' @param force_of_infection A matrix representing the force of infection where
#' the row dimension corresponds to the `times` given and the column dimension
#' corresponds to the `strata` given or `NULL` to generate a random one.
#' @param force_of_infection_mean The mean of the initial force of infection to
#' use. Only used when `force_of_infection` is `NULL`.
#' @param seed The random seed to use for generating the data set.
#'
#' @return
#' A data.frame with the columns 'patient', 'time', 'detection', 'outcome'
#' along with the strata columns provided in `strata`.
#'
#' @importFrom checkmate assert
#' @importFrom checkmate check_date
#' @importFrom checkmate check_matrix
#' @importFrom checkmate check_null
#' @importFrom checkmate check_vector
#' @importFrom stats rmultinom
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @export
create_sample_linelist <- function(
    strata,
    times,
    active_detection,
    passive_asymptomatic_detection,
    passive_symptomatic_detection,
    force_of_infection = NULL,
    force_of_infection_mean = -5.0,
    seed = 1L) {
  # Input validation
  set.seed(seed = seed)
  strata <- is_data_frame(strata)
  missing_columns <- setdiff(c("population", "sir", "ifr"), names(strata))
  if (length(missing_columns)) {
    stop(
      "`strata` is missing required columns: ",
      paste0(missing_columns, collapse = ","),
      "."
    )
  }
  checkmate::assert(
    checkmate::check_vector(
      times,
      strict = TRUE,
      any.missing = FALSE,
      min.len = 1L,
      unique = TRUE
    ),
    checkmate::check_date(
      times,
      any.missing = FALSE,
      min.len = 1L,
      unique = TRUE
    )
  )
  assert_probability(active_detection)
  assert_probability(passive_asymptomatic_detection)
  assert_probability(passive_symptomatic_detection)
  checkmate::assert(
    checkmate::check_matrix(
      force_of_infection,
      mode = "numeric",
      any.missing = FALSE,
      nrows = length(times),
      ncols = nrow(strata)
    ),
    checkmate::check_null(force_of_infection)
  )

  # Wrangle `strata`
  strata_cols <- setdiff(names(strata), c("population", "sir", "ifr"))
  if (length(strata_cols) == 0L) {
    stop("Not providing any strata is temporarily not supported.")
  }
  invalid_cols <- intersect(
    strata_cols, c("detection", "outcome", "patient", "time")
  )
  if (length(invalid_cols) > 0L) {
    stop(
      "The following strata columns are not valid: ",
      paste0(invalid_cols, collapse = ", "),
      "."
    )
  }

  # Calculate force of infection if not provided
  if (is.null(force_of_infection)) {
    force_of_infection <- matrix(nrow = length(times), ncol = nrow(strata))
    for (i in seq_along(times)) {
      if (i == 1L) {
        force_of_infection[1, ] <- stats::rnorm(
          nrow(strata), mean = force_of_infection_mean, sd = 1.0
        )
      } else {
        force_of_infection[i, ] <- stats::rnorm(
          nrow(strata),
          mean = force_of_infection[i - 1L, ], sd = 1.0
        )
      }
    }
    force_of_infection <- inv_logit(force_of_infection)
  }

  # Calculate the susceptible/case saturation matrices
  susceptible <- matrix(nrow = length(times), ncol = nrow(strata))
  case_saturation <- matrix(nrow = length(times), ncol = nrow(strata))
  for (i in seq_along(times)) {
    for (j in seq_len(nrow(strata))) {
      if (i == 1L) {
        susceptible[1L, j] <- strata[j, "population"]
        case_saturation[1L, j] <- strata[j, "population"] *
          force_of_infection[1L, j]
      } else {
        susceptible[i, j] <- susceptible[i - 1L, j] - case_saturation[i - 1L, j]
        case_saturation[i, j] <- susceptible[i, j] * force_of_infection[i, j]
      }
    }
  }

  # Calculate the observed incidences
  incidence <- array(dim = c(length(times), 2L, nrow(strata)))
  inv_active_detection <- 1.0 - active_detection
  for (i in seq_along(times)) {
    for (j in seq_len(nrow(strata))) {
      sir <- strata[j, "sir"]
      incidence[i, 1L, j] <- stats::rpois(
        1L, active_detection * case_saturation[i, j]
      )
      sym_lambda <- inv_active_detection *
        ((passive_asymptomatic_detection * (1.0 - sir)) +
           (passive_symptomatic_detection * sir))
      incidence[i, 2L, j] <- stats::rpois(
        1L, sym_lambda * case_saturation[i, j]
      )
    }
  }

  # Create grid to loop over
  index_grid <- apply(
    expand.grid(
      time_idx = seq_along(times),
      strata_idx = seq_len(nrow(strata)),
      detection_idx = c(1L, 2L),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    1L,
    as.list
  )

  # Create a linelist
  linelist <- do.call(rbind, lapply(index_grid, function(x) {
    linelist_part <- data.frame()
    obs_incidence <- incidence[x$time_idx, x$detection_idx, x$strata_idx]
    if (obs_incidence == 0L) {
      return(data.frame())
    }
    obs_sir <- sir <- strata[x$strata_idx, "sir"]
    obs_ifr <- ifr <- strata[x$strata_idx, "ifr"]
    if (x$detection_idx == 2L) {
      denom <- 1.0 -
        ((1.0 - ifr) *
           (1.0 - ((passive_asymptomatic_detection * (1.0 - sir)) +
                     (passive_symptomatic_detection * sir))))
      obs_sir <- (1.0 -
                    ((1.0 - ifr) *
                       (1.0 - (passive_symptomatic_detection * sir)))) / denom
      obs_ifr <- ifr / denom
    }
    outcome <- rep(
      c("asymptomatic", "symptomatic", "death"),
      times = c(
        stats::rmultinom(
          1L, obs_incidence, c(1.0 - obs_sir, obs_sir - obs_ifr, obs_ifr)
        )
      )
    )
    linelist_part <- data.frame(outcome = outcome)
    linelist_part$detection <- ifelse(
      x$detection_idx == 1L, "active", "passive"
    )
    linelist_part$time <- times[x$time_idx]
    linelist_part[, strata_cols] <- strata[
      x$strata_idx, strata_cols, drop = FALSE
    ]
    linelist_part
  }))

  # Linelist formatting
  linelist$patient <- paste0("UID", seq_len(nrow(linelist)))
  cols <- c("patient", "time", strata_cols, "detection", "outcome")
  linelist <- linelist[, cols]

  # Return
  linelist
}
