test_that("Non-SeverityEstimateFit or list object given for `x`", {
  lapply(list(NULL, data.frame(), Sys.time()), function(x) {
    expect_error(
      calculate_fatality_ratio(x),
      regexp = paste0(
        "Unable to find a suitable `calculate_fatality_ratio` method for ",
        "`x` with classes: ",
        toString(class(x)),
        "."
      ),
      fixed = TRUE
    )
  })
})

test_that("Input validation when given a list for `x`", {
  # Reusable valid inputs for testing
  samples <- 100L
  time_steps <- 20L
  unique_strata <- 5L
  valid_x <- list(
    "C" = array(
      data = rpois(samples * time_steps * unique_strata, 30.0),
      dim = c(samples, time_steps, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "mortality" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "xi" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    )
  )

  # Test that `x` has the names "C", "mortality", & "xi"
  expect_error(
    calculate_fatality_ratio(list("abc" = letters, "def" = LETTERS)),
    regexp = 'all(c("C", "mortality", "xi") %in% names(x)) is not TRUE',
    fixed = TRUE
  )
  expect_error(
    calculate_fatality_ratio(list("C" = 1L:10L, "xi" = 11L:20L)),
    regexp = 'all(c("C", "mortality", "xi") %in% names(x)) is not TRUE',
    fixed = TRUE
  )

  # Test mean/median/naive estimate single logical parameters
  lapply(list(NA, NULL, c(TRUE, TRUE), 1L), function(x) {
    expect_error(calculate_fatality_ratio(valid_x, mean_estimate = x))
    expect_error(calculate_fatality_ratio(valid_x, median_estimate = x))
    expect_error(calculate_fatality_ratio(valid_x, naive_estimate = x))
  })
})

test_that("Output validation when given a list for `x`", {
  # Sample input
  samples <- 100L
  time_steps <- 20L
  unique_strata <- 5L
  strata <- data.frame(
    "age_group" = c(
      "Children",
      "Elderly",
      "Infants",
      "Working Adults",
      "Young Adults"
    )
  )
  x <- list(
    "C" = array(
      data = rpois(samples * time_steps * unique_strata, 30.0),
      dim = c(samples, time_steps, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "mortality" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "xi" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    )
  )
  incident_entries <- time_steps * unique_strata * 3L * 3L
  incidence <- array(
    data = rbinom(incident_entries, 1L, 0.1) * rpois(incident_entries, 20.0),
    dim = c(time_steps, unique_strata, 3L, 3L)
  )
  outcome <- data.frame(
    patient_status = c("Asymptomatic", "Death", "Symptomatic")
  )

  # Basic test, just mean/median IFR/SIR estimates
  fatality_ratios <- calculate_fatality_ratio(x, strata, alpha = numeric())
  expected_fatality_ratios <- strata
  expected_fatality_ratios$ifr_mean_estimate <- apply(x$mortality, 2L, mean)
  expected_fatality_ratios$ifr_median_estimate <- apply(x$mortality, 2L, median)
  expected_fatality_ratios$sir_mean_estimate <- apply(x$xi, 2L, mean)
  expected_fatality_ratios$sir_median_estimate <- apply(x$xi, 2L, median)
  expect_identical(fatality_ratios, expected_fatality_ratios)

  # Expanded test with intervals
  fatality_ratios <- calculate_fatality_ratio(x, strata)
  expected_fatality_ratios <- strata
  expected_fatality_ratios$ifr_mean_estimate <- apply(x$mortality, 2L, mean)
  expected_fatality_ratios$ifr_median_estimate <- apply(x$mortality, 2L, median)
  expected_fatality_ratios$ifr_lower_05 <- apply(
    x$mortality,
    2L,
    stats::quantile,
    probs = 0.025,
    names = FALSE
  )
  expected_fatality_ratios$ifr_upper_05 <- apply(
    x$mortality,
    2L,
    stats::quantile,
    probs = 0.975,
    names = FALSE
  )
  expected_fatality_ratios$sir_mean_estimate <- apply(x$xi, 2L, mean)
  expected_fatality_ratios$sir_median_estimate <- apply(x$xi, 2L, median)
  expected_fatality_ratios$sir_lower_05 <- apply(
    x$xi,
    2L,
    stats::quantile,
    probs = 0.025,
    names = FALSE
  )
  expected_fatality_ratios$sir_upper_05 <- apply(
    x$xi,
    2L,
    stats::quantile,
    probs = 0.975,
    names = FALSE
  )
  expect_identical(fatality_ratios, expected_fatality_ratios)

  # Naive estimates
  fatality_ratios <- calculate_fatality_ratio(
    x,
    strata,
    naive_estimate = TRUE,
    alpha = numeric(),
    incidence = incidence,
    outcome = outcome
  )
  expected_fatality_ratios <- strata
  expected_fatality_ratios$ifr_mean_estimate <- apply(x$mortality, 2L, mean)
  expected_fatality_ratios$ifr_median_estimate <- apply(x$mortality, 2L, median)
  expected_fatality_ratios$sir_mean_estimate <- apply(x$xi, 2L, mean)
  expected_fatality_ratios$sir_median_estimate <- apply(x$xi, 2L, median)
  expected_fatality_ratios$naive_ifr <- (apply(incidence[,,, 2L], 2L, sum) /
    apply(incidence, 2L, sum))
  expected_fatality_ratios$naive_sir <- (apply(incidence[,,, 2L:3L], 2L, sum) /
    apply(incidence, 2L, sum))
  expect_identical(fatality_ratios, expected_fatality_ratios)
})

test_that("Output validation when given a list for `x` and missing outcomes", {
  # Sample input with no deaths
  samples <- 100L
  time_steps <- 20L
  unique_strata <- 5L
  strata <- data.frame(
    "age_group" = c(
      "Children",
      "Elderly",
      "Infants",
      "Working Adults",
      "Young Adults"
    )
  )
  x <- list(
    "C" = array(
      data = rpois(samples * time_steps * unique_strata, 30.0),
      dim = c(samples, time_steps, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "mortality" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "xi" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    )
  )
  incident_entries <- time_steps * unique_strata * 3L * 2L
  incidence <- array(
    data = rbinom(incident_entries, 1L, 0.1) * rpois(incident_entries, 20.0),
    dim = c(time_steps, unique_strata, 3L, 2L)
  )
  outcome <- data.frame(
    patient_status = c("Asymptomatic", "Symptomatic")
  )

  # Naive estimates
  fatality_ratios <- calculate_fatality_ratio(
    x,
    strata,
    naive_estimate = TRUE,
    alpha = numeric(),
    incidence = incidence,
    outcome = outcome
  )
  expected_fatality_ratios <- strata
  expected_fatality_ratios$ifr_mean_estimate <- apply(x$mortality, 2L, mean)
  expected_fatality_ratios$ifr_median_estimate <- apply(x$mortality, 2L, median)
  expected_fatality_ratios$sir_mean_estimate <- apply(x$xi, 2L, mean)
  expected_fatality_ratios$sir_median_estimate <- apply(x$xi, 2L, median)
  expected_fatality_ratios$naive_ifr <- rep.int(0.0, unique_strata)
  expected_fatality_ratios$naive_sir <- (apply(incidence[,,, 2L], 2L, sum) /
    apply(incidence, 2L, sum))
  expect_identical(fatality_ratios, expected_fatality_ratios)

  # Sample input with no deaths or symptoms
  samples <- 100L
  time_steps <- 20L
  unique_strata <- 5L
  strata <- data.frame(
    "age_group" = c(
      "Children",
      "Elderly",
      "Infants",
      "Working Adults",
      "Young Adults"
    )
  )
  x <- list(
    "C" = array(
      data = rpois(samples * time_steps * unique_strata, 30.0),
      dim = c(samples, time_steps, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "mortality" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    ),
    "xi" = array(
      data = runif(samples * unique_strata),
      dim = c(samples, unique_strata),
      dimnames = list("iterations" = NULL)
    )
  )
  incident_entries <- time_steps * unique_strata * 3L * 1L
  incidence <- array(
    data = rbinom(incident_entries, 1L, 0.1) * rpois(incident_entries, 20.0),
    dim = c(time_steps, unique_strata, 3L, 1L)
  )
  outcome <- data.frame(
    patient_status = c("Asymptomatic")
  )

  # Naive estimates
  fatality_ratios <- calculate_fatality_ratio(
    x,
    strata,
    naive_estimate = TRUE,
    alpha = numeric(),
    incidence = incidence,
    outcome = outcome
  )
  expected_fatality_ratios <- strata
  expected_fatality_ratios$ifr_mean_estimate <- apply(x$mortality, 2L, mean)
  expected_fatality_ratios$ifr_median_estimate <- apply(x$mortality, 2L, median)
  expected_fatality_ratios$sir_mean_estimate <- apply(x$xi, 2L, mean)
  expected_fatality_ratios$sir_median_estimate <- apply(x$xi, 2L, median)
  expected_fatality_ratios$naive_ifr <- rep.int(0.0, unique_strata)
  expected_fatality_ratios$naive_sir <- rep.int(0.0, unique_strata)
  expect_identical(fatality_ratios, expected_fatality_ratios)
})
