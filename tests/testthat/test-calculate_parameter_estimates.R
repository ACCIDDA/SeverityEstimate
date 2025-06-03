test_that("Non-SeverityEstimateFit or list object given for `x`", {
  expect_error(
    calculate_parameter_estimates(NULL),
    regexp = paste0(
      "Unable to find a suitable `calculate_parameter_estimates` method for ",
      "`x` with classes: NULL."
    ),
    fixed = TRUE
  )
  expect_error(
    calculate_parameter_estimates(data.frame()),
    regexp = paste0(
      "Unable to find a suitable `calculate_parameter_estimates` method for ",
      "`x` with classes: data.frame."
    ),
    fixed = TRUE
  )
  expect_error(
    calculate_parameter_estimates(Sys.time()),
    regexp = paste0(
      "Unable to find a suitable `calculate_parameter_estimates` method for ",
      "`x` with classes: POSIXct, POSIXt."
    ),
    fixed = TRUE
  )
})

test_that("Input validation when given a list for `x`", {
  # Setup
  x <- list(
    "active_detection" = array(data = seq(0.1, 0.3, by = 0.1)),
    "passive_asymptomatic_detection" = array(data = seq(0.1, 0.3, by = 0.1)),
    "passive_symptomatic_detection" = array(data = seq(0.1, 0.3, by = 0.1))
  )

  # Tests
  expect_error(calculate_parameter_estimates(list()))
  expect_error(calculate_parameter_estimates(list("phi" = 1L:10L)))
  expect_error(calculate_parameter_estimates(x, mean_estimate = NULL))
  expect_error(calculate_parameter_estimates(x, mean_estimate = NA))
  expect_error(
    calculate_parameter_estimates(x, mean_estimate = c(TRUE, FALSE))
  )
  expect_error(calculate_parameter_estimates(x, median_estimate = NULL))
  expect_error(calculate_parameter_estimates(x, median_estimate = NA))
  expect_error(
    calculate_parameter_estimates(x, median_estimate = c(TRUE, FALSE))
  )
  expect_error(calculate_parameter_estimates(x, include_description = NULL))
  expect_error(calculate_parameter_estimates(x, include_description = NA))
  expect_error(
    calculate_parameter_estimates(x, include_description = c(TRUE, FALSE))
  )

  # Specific error
  expect_error(
    calculate_parameter_estimates(
      x,
      mean_estimate = FALSE,
      median_estimate = FALSE,
      alpha = numeric()
    ),
    regexp = paste0(
      "At least one of following must be true: `mean_estimate` is `TRUE`, ",
      "`median_estimate` is `TRUE`, or `alpha` is non-empty."
    ),
    fixed = TRUE
  )
})

test_that("Output validation when given a list for `x`", {
  # Test one
  x1 <- list(
    "active_detection" = array(data = c(0.03, 0.92, 0.52, 0.51)),
    "passive_asymptomatic_detection" = array(data = c(0.99, 0.06, 0.21, 0.35)),
    "passive_symptomatic_detection" = array(data = c(0.51, 0.42, 0.17, 0.42))
  )
  expected_df1 <- data.frame(
    parameter = c(
      "active_detection",
      "passive_asymptomatic_detection",
      "passive_symptomatic_detection"
    ),
    parameter_description = c(
      "active detection rate",
      "mildly/asymptomatic passive detection rate",
      "severe symptoms passive detection rate"
    ),
    mean_estimate = c(
      mean(x1$active_detection),
      mean(x1$passive_asymptomatic_detection),
      mean(x1$passive_symptomatic_detection)
    ),
    median_estimate = c(
      median(x1$active_detection),
      median(x1$passive_asymptomatic_detection),
      median(x1$passive_symptomatic_detection)
    )
  )

  df1 <- calculate_parameter_estimates(x1, alpha = numeric())
  expect_identical(df1, expected_df1)

  # Test two
  x2 <- list(
    "active_detection" = array(data = runif(100L)),
    "passive_asymptomatic_detection" = array(data = runif(100L)),
    "passive_symptomatic_detection" = array(data = runif(100L))
  )
  expected_df2 <- data.frame(
    parameter = c(
      "active_detection",
      "passive_asymptomatic_detection",
      "passive_symptomatic_detection"
    ),
    mean_estimate = c(
      mean(x2$active_detection),
      mean(x2$passive_asymptomatic_detection),
      mean(x2$passive_symptomatic_detection)
    ),
    median_estimate = c(
      median(x2$active_detection),
      median(x2$passive_asymptomatic_detection),
      median(x2$passive_symptomatic_detection)
    ),
    lower_05 = c(
      stats::quantile(x2$active_detection, probs = 0.025, names = FALSE),
      stats::quantile(
        x2$passive_asymptomatic_detection, probs = 0.025, names = FALSE
      ),
      stats::quantile(
        x2$passive_symptomatic_detection, probs = 0.025, names = FALSE
      )
    ),
    upper_05 = c(
      stats::quantile(x2$active_detection, probs = 0.975, names = FALSE),
      stats::quantile(
        x2$passive_asymptomatic_detection, probs = 0.975, names = FALSE
      ),
      stats::quantile(
        x2$passive_symptomatic_detection, probs = 0.975, names = FALSE
      )
    )
  )

  df2 <- calculate_parameter_estimates(
    x2,
    alpha = 0.05,
    include_description = FALSE
  )
  expect_identical(df2, expected_df2)

  # Test three
  modified_tanh <- \(x) 0.5 * (tanh(x) + 1.0)
  x3 <- list(
    "active_detection" = array(data = modified_tanh(rnorm(1000L))),
    "passive_asymptomatic_detection" = modified_tanh(
      rnorm(1000L, mean = 2.0, sd = 1.5)
    ),
    "passive_symptomatic_detection" = modified_tanh(
      rnorm(1000L, mean = -3.0, sd = 0.75)
    )
  )
  expected_df3 <- data.frame(
    parameter = c(
      "active_detection",
      "passive_asymptomatic_detection",
      "passive_symptomatic_detection"
    ),
    mean_estimate = c(
      mean(x3$active_detection),
      mean(x3$passive_asymptomatic_detection),
      mean(x3$passive_symptomatic_detection)
    ),
    lower_05 = c(
      stats::quantile(x3$active_detection, probs = 0.025, names = FALSE),
      stats::quantile(
        x3$passive_asymptomatic_detection, probs = 0.025, names = FALSE
      ),
      stats::quantile(
        x3$passive_symptomatic_detection, probs = 0.025, names = FALSE
      )
    ),
    upper_05 = c(
      stats::quantile(x3$active_detection, probs = 0.975, names = FALSE),
      stats::quantile(
        x3$passive_asymptomatic_detection, probs = 0.975, names = FALSE
      ),
      stats::quantile(
        x3$passive_symptomatic_detection, probs = 0.975, names = FALSE
      )
    ),
    lower_01 = c(
      stats::quantile(x3$active_detection, probs = 0.005, names = FALSE),
      stats::quantile(
        x3$passive_asymptomatic_detection, probs = 0.005, names = FALSE
      ),
      stats::quantile(
        x3$passive_symptomatic_detection, probs = 0.005, names = FALSE
      )
    ),
    upper_01 = c(
      stats::quantile(x3$active_detection, probs = 0.995, names = FALSE),
      stats::quantile(
        x3$passive_asymptomatic_detection, probs = 0.995, names = FALSE
      ),
      stats::quantile(
        x3$passive_symptomatic_detection, probs = 0.995, names = FALSE
      )
    )
  )

  df3 <- calculate_parameter_estimates(
    x3,
    median_estimate = FALSE,
    alpha = c(0.05, 0.01),
    include_description = FALSE,
  )
  expect_identical(df3, expected_df3)
})
