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
#' @param case_saturation_from The lower bound on rates to draw from when
#' initializing case saturation along the population.
#' @param case_saturation_to The upper bound on rates to draw from when
#' initializing case saturation along the population.
#' @param stochastic_case_saturation A single logical indicating if the case
#' should be stochastic in time or not.
#' point.
#' @param seed The random seed to use for generating the data set.
#'
#' @return
#' A data.frame with the columns 'patient', 'time', 'detection', 'outcome'
#' along with the strata columns provided in `strata`.
#'
#' @importFrom checkmate assert
#' @importFrom checkmate check_date
#' @importFrom checkmate check_vector
#' @export
create_sample_linelist <- function(
    strata,
    times,
    active_detection,
    passive_asymptomatic_detection,
    passive_symptomatic_detection,
    case_saturation_from = 500.0,
    case_saturation_to = 500.0,
    stochastic_case_saturation = FALSE,
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
  checkmate::assert_number(case_saturation_from, lower = 0.0, finite = TRUE)
  checkmate::assert_number(case_saturation_to, lower = 0.0, finite = TRUE)
  checkmate::assert_logical(
    stochastic_case_saturation, any.missing = FALSE, len = 1L
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

  # Create our matrices
  nstrata <- nrow(strata)
  ntime <- length(times)
  susceptible <- matrix(nrow = ntime, ncol = nstrata)
  case_saturation <- matrix(nrow = ntime, ncol = nstrata)

  # Populate these matrices
  case_saturation[1L, ] <- strata[, "population"] * rexp(
    nstrata,
    rate = seq(
      from = case_saturation_from,
      to = case_saturation_to,
      length.out = nstrata
    )
  )
  susceptible[1L, ] <- strata[, "population"] - case_saturation[1L, ]
  for (i in 2L:ntime) {
    if (stochastic_case_saturation) {
      case_saturation[i, ] <- rnorm(
        nstrata,
        mean = case_saturation[i - 1, ],
        sd = 0.01 * case_saturation[i - 1, ]
      )
      susceptible[i, ] <- strata[, "population"] - case_saturation[i, ]
    } else {
      case_saturation[i, ] <- case_saturation[i - 1, ]
      susceptible[i, ] <- case_saturation[i - 1, ]
    }
  }

  infections_active <- matrix(
    data = rpois(ntime * nstrata, lambda = active_detection * case_saturation),
    nrow = ntime,
    ncol = nstrata,
    dimnames = list(
      1L:ntime,
      1L:nstrata
    )
  )
  xi <- matrix(
    data = rep(strata[, "sir"], times = rep(ntime, nstrata)),
    nrow = ntime,
    ncol = nstrata
  )
  passive_lambda <- passive_asymptomatic_detection * (1.0 * xi)
  passive_lambda <- passive_lambda + (passive_symptomatic_detection * xi)
  passive_lambda <- (1.0 - active_detection) * passive_lambda
  infections_passive <- matrix(
    data = rpois(ntime * nstrata, lambda = passive_lambda * case_saturation),
    nrow = ntime,
    ncol = nstrata,
    dimnames = list(
      1L:ntime,
      1L:nstrata
    )
  )

  # Unpack the observation matrices into a data.frame
  passive_strata <- passive_from_active_strata(
    strata,
    active_detection,
    passive_asymptomatic_detection,
    passive_symptomatic_detection
  )
  index_grid <- apply(
    data.frame(
      i = rep_len(1L:ntime, ntime * nstrata),
      j = rep_len(1L:nstrata, ntime * nstrata)
    ),
    1L,
    as.list
  )
  linelist <- do.call(rbind, lapply(index_grid, function(x) {
    linelist_part <- data.frame()
    active_ij <- infections_active[x$i, x$j]
    if (active_ij > 0L) {
      sir <- strata[x$j, "sir"]
      ifr <- strata[x$j, "ifr"]
      outcome <- ifelse(runif(active_ij) < sir, "symptomatic", "asymptomatic")
      outcome <- ifelse(
        outcome == "symptomatic" & runif(active_ij) < ifr, "death", outcome
      )
      linelist_part <- rbind(linelist_part, data.frame(
        detection = rep_len("active", active_ij),
        outcome = outcome
      ))
    }
    passive_ij <- infections_passive[x$i, x$j]
    if (passive_ij > 0L) {
      sir <- passive_strata[x$j, "sir"]
      ifr <- passive_strata[x$j, "ifr"]
      outcome <- ifelse(runif(passive_ij) < sir, "symptomatic", "asymptomatic")
      outcome <- ifelse(
        outcome == "symptomatic" & runif(passive_ij) < ifr, "death", outcome
      )
      linelist_part <- rbind(linelist_part, data.frame(
        detection = rep_len("passive", passive_ij),
        outcome = outcome
      ))
    }
    if (nrow(linelist_part) == 0L) {
      return(linelist_part)
    }
    linelist_part$time <- times[x$i]
    linelist_part[, strata_cols] <- strata[x$j, strata_cols, drop = FALSE]
    linelist_part
  }))
  linelist$patient <- paste0("UID", seq_len(nrow(linelist)))
  cols <- c("patient", "time", strata_cols, "detection", "outcome")
  linelist <- linelist[, cols]

  # Return
  linelist
}
