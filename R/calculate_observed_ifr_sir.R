#' @title
#' Calculate Active/Passive IFR/SIR From True IFR/SIR And Detection Rates
#'
#' @inheritParams create_sample_linelist
#'
#' @return
#' The given `strata` data.frame with the additional columns 'theta',
#' 'active_sir', 'passive_sir', 'active_ifr', and 'passive_ifr'.
#'
#' @export
calculate_observed_ifr_sir <- function(
  strata,
  passive_asymptomatic_detection,
  passive_symptomatic_detection
) {
  # Input validation
  assert_probability(passive_asymptomatic_detection)
  assert_probability(passive_symptomatic_detection)

  # Calculation
  active_sir <- strata[, "sir"]
  active_ifr <- strata[, "ifr"]
  theta <- (passive_asymptomatic_detection * (1.0 - active_sir)) +
    (passive_symptomatic_detection * active_sir)
  denom <- 1.0 - ((1.0 - active_ifr) * (1.0 - theta))
  passive_sir <- (1.0 -
                    ((1.0 - active_ifr) *
                       (1.0 - (passive_symptomatic_detection * active_sir)))) /
    denom
  passive_ifr <- active_ifr / denom

  # Formatting
  strata$theta <- theta
  strata$active_sir <- active_sir
  strata$passive_sir <- passive_sir
  strata$active_ifr <- active_ifr
  strata$passive_ifr <- passive_ifr
  strata
}
