#' @title
#' Calculate The Passively Observed SIR, IFR From Actively Observed SIR, IFR
#'
#' @inheritParams create_sample_linelist
#'
#' @return
#' A data.frame of the same structure as `strata` but with the 'sir' and 'ifr'
#' columns adjusted for being passively observed.
#'
#' @keywords internal
passive_from_active_strata <- function(
    strata,
    active_detection,
    passive_asymptomatic_detection,
    passive_symptomatic_detection) {
  # Setup
  passive_strata <- strata

  # Loop over
  for (i in seq_len(nrow(passive_strata))) {
    sir <- strata[i, "sir"]
    ifr <- strata[i, "ifr"]

    tmp <- (1 - sir) * passive_asymptomatic_detection
    tmp <- tmp + (sir * passive_symptomatic_detection)

    adj_sir <- (1 - ((1 - (passive_symptomatic_detection * sir)) * (1 - ifr)))
    adj_sir <- adj_sir / (1 - (1 - ifr) * (1 - tmp))

    adj_ifr <- ifr / (1 - ((1 - ifr) * (1 - tmp)))

    passive_strata[i, "sir"] <- adj_sir
    passive_strata[i, "ifr"] <- adj_ifr
  }

  # Return
  passive_strata
}
