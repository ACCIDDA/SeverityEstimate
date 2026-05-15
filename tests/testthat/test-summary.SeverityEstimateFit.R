test_that("summary.SeverityEstimateFit wraps helper summaries", {
  helper_calls <- new.env(parent = emptyenv())
  raw_detection_rates <- data.frame(
    parameter = c(
      "active_detection",
      "passive_asymptomatic_detection",
      "passive_symptomatic_detection"
    ),
    mean_estimate = c(0.30, 0.10, 0.20)
  )
  raw_severity_estimates <- data.frame(
    age = c("youth", "adult"),
    health_occupation = c("no", "yes"),
    ifr_mean_estimate = c(0.01, 0.02),
    sir_mean_estimate = c(0.20, 0.30)
  )

  local_mocked_bindings(
    calculate_parameter_estimates = function(
      x,
      mean_estimate,
      median_estimate,
      alpha,
      include_description,
      ...
    ) {
      helper_calls$detection <- list(
        x = x,
        mean_estimate = mean_estimate,
        median_estimate = median_estimate,
        alpha = alpha,
        include_description = include_description
      )
      raw_detection_rates
    },
    calculate_fatality_ratio = function(
      x,
      mean_estimate,
      median_estimate,
      naive_estimate,
      alpha,
      ...
    ) {
      helper_calls$severity <- list(
        x = x,
        mean_estimate = mean_estimate,
        median_estimate = median_estimate,
        naive_estimate = naive_estimate,
        alpha = alpha
      )
      raw_severity_estimates
    },
    .package = "SeverityEstimate"
  )

  fit_summary <- summary(structure(
    list("id" = 1L),
    class = "SeverityEstimateFit"
  ))

  expect_s3_class(fit_summary, "SummaryEstimateFit")
  expect_identical(helper_calls$detection$x$id, 1L)
  expect_true(helper_calls$detection$mean_estimate)
  expect_false(helper_calls$detection$median_estimate)
  expect_identical(helper_calls$detection$alpha, numeric())
  expect_false(helper_calls$detection$include_description)
  expect_identical(helper_calls$severity$x$id, 1L)
  expect_true(helper_calls$severity$mean_estimate)
  expect_false(helper_calls$severity$median_estimate)
  expect_false(helper_calls$severity$naive_estimate)
  expect_identical(helper_calls$severity$alpha, numeric())
  expect_identical(
    fit_summary$detection_rates,
    data.frame(
      Estimate = c(0.10, 0.20, 0.30),
      row.names = c(
        "passive_asymptomatic",
        "passive_symptomatic",
        "active"
      ),
      check.names = FALSE
    )
  )
  expect_identical(
    fit_summary$severity_estimates,
    data.frame(
      age = c("youth", "adult"),
      health_occupation = c("no", "yes"),
      "IFR Estimate" = c(0.01, 0.02),
      "SIR Estimate" = c(0.20, 0.30),
      check.names = FALSE
    )
  )
})

test_that("summary.SeverityEstimateFit handles the no-strata case", {
  local_mocked_bindings(
    calculate_parameter_estimates = function(...) {
      data.frame(
        parameter = c(
          "active_detection",
          "passive_asymptomatic_detection",
          "passive_symptomatic_detection"
        ),
        mean_estimate = c(0.30, 0.10, 0.20)
      )
    },
    calculate_fatality_ratio = function(...) {
      data.frame(
        ifr_mean_estimate = 0.01,
        sir_mean_estimate = 0.20
      )
    },
    .package = "SeverityEstimate"
  )

  fit_summary <- summary(structure(list(), class = "SeverityEstimateFit"))

  expect_identical(
    fit_summary$severity_estimates,
    data.frame(
      "IFR Estimate" = 0.01,
      "SIR Estimate" = 0.20,
      check.names = FALSE
    )
  )
})

test_that("format_summary_severity_estimates reorders by strata reference", {
  severity_estimates <- data.frame(
    age = c("adult", "senior", "youth", "adult", "senior", "youth"),
    health_occupation = c("yes", "no", "yes", "no", "yes", "no"),
    ifr_mean_estimate = c(4, 5, 2, 3, 6, 1),
    sir_mean_estimate = c(104, 105, 102, 103, 106, 101)
  )
  attr(
    severity_estimates,
    "strata_reference"
  ) <- data.frame(
    age = rep(c("youth", "adult", "senior"), each = 2L),
    health_occupation = rep(c("no", "yes"), times = 3L)
  )

  expect_identical(
    format_summary_severity_estimates(severity_estimates),
    data.frame(
      age = rep(c("youth", "adult", "senior"), each = 2L),
      health_occupation = rep(c("no", "yes"), times = 3L),
      "IFR Estimate" = as.numeric(1:6),
      "SIR Estimate" = as.numeric(101:106),
      check.names = FALSE
    )
  )
})

test_that("print.SummaryEstimateFit prints the summary tables", {
  fit_summary <- structure(
    list(
      detection_rates = data.frame(
        Estimate = c(0.10, 0.20, 0.30),
        row.names = c(
          "passive_asymptomatic",
          "passive_symptomatic",
          "active"
        ),
        check.names = FALSE
      ),
      severity_estimates = data.frame(
        age = c("youth", "adult"),
        health_occupation = c("no", "yes"),
        "IFR Estimate" = c(0.01, 0.02),
        "SIR Estimate" = c(0.20, 0.30),
        check.names = FALSE
      )
    ),
    class = "SummaryEstimateFit"
  )

  printed_output <- utils::capture.output(
    # jarl-ignore implicit_assignment: capture output for testing
    returned_value <- print(fit_summary, digits = 3L)
  )

  expect_identical(returned_value, fit_summary)
  expect_true(any(grepl("^Detection Rates:$", printed_output)))
  expect_true(any(grepl("passive_asymptomatic", printed_output, fixed = TRUE)))
  expect_true(any(grepl("^Severity Estimates:$", printed_output)))
  expect_true(any(grepl("health_occupation", printed_output, fixed = TRUE)))
  expect_true(any(grepl("IFR Estimate", printed_output, fixed = TRUE)))
  expect_true(any(grepl("SIR Estimate", printed_output, fixed = TRUE)))
})
