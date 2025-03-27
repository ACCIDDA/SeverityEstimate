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
    "phi" = array(data = seq(0.1, 0.3, by = 0.1)),
    "psi" = array(data = seq(0.1, 0.6, by = 0.1), dim = c(3L, 2L))
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
    "phi" = array(data = c(0.03, 0.92, 0.52, 0.51)),
    "psi" = array(
      data = c(
        0.99, 0.06, 0.21, 0.35,
        0.51, 0.42, 0.17, 0.42
      ),
      dim = c(4L, 2L)
    )
  )
  expected_df1 <- data.frame(
    parameter = c("psi_1", "psi_2", "phi"),
    parameter_description = c(
      "mildly/asymptomatic passive detection rate",
      "severe symptoms passive detection rate",
      "active detection rate"
    ),
    mean_estimate = c(mean(x1$psi[, 1]), mean(x1$psi[, 2]), mean(x1$phi)),
    median_estimate = c(
      median(x1$psi[, 1]),
      median(x1$psi[, 2]),
      median(x1$phi)
    )
  )

  df1 <- calculate_parameter_estimates(x1)
  expect_identical(df1, expected_df1)

  # Test two
  x2 <- list(
    "phi" = array(data = runif(100L)),
    "psi" = array(
      data = runif(200L),
      dim = c(100L, 2L)
    )
  )
  expected_df2 <- data.frame(
    parameter = c("psi_1", "psi_2", "phi"),
    mean_estimate = c(mean(x2$psi[, 1]), mean(x2$psi[, 2]), mean(x2$phi)),
    median_estimate = c(
      median(x2$psi[, 1]),
      median(x2$psi[, 2]),
      median(x2$phi)
    ),
    lower_05 = c(
      stats::quantile(x2$psi[, 1], probs = 0.025, names = FALSE),
      stats::quantile(x2$psi[, 2], probs = 0.025, names = FALSE),
      stats::quantile(x2$phi, probs = 0.025, names = FALSE)
    ),
    upper_05 = c(
      stats::quantile(x2$psi[, 1], probs = 0.975, names = FALSE),
      stats::quantile(x2$psi[, 2], probs = 0.975, names = FALSE),
      stats::quantile(x2$phi, probs = 0.975, names = FALSE)
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
    "phi" = array(data = modified_tanh(rnorm(1000L))),
    "psi" = array(
      data = c(
        modified_tanh(rnorm(1000L, mean = 2.0, sd = 1.5)),
        modified_tanh(rnorm(1000L, mean = -3.0, sd = 0.75))
      ),
      dim = c(1000L, 2L)
    )
  )
  expected_df3 <- data.frame(
    parameter = c("psi_1", "psi_2", "phi"),
    mean_estimate = c(mean(x3$psi[, 1]), mean(x3$psi[, 2]), mean(x3$phi)),
    lower_05 = c(
      stats::quantile(x3$psi[, 1], probs = 0.025, names = FALSE),
      stats::quantile(x3$psi[, 2], probs = 0.025, names = FALSE),
      stats::quantile(x3$phi, probs = 0.025, names = FALSE)
    ),
    upper_05 = c(
      stats::quantile(x3$psi[, 1], probs = 0.975, names = FALSE),
      stats::quantile(x3$psi[, 2], probs = 0.975, names = FALSE),
      stats::quantile(x3$phi, probs = 0.975, names = FALSE)
    ),
    lower_01 = c(
      stats::quantile(x3$psi[, 1], probs = 0.005, names = FALSE),
      stats::quantile(x3$psi[, 2], probs = 0.005, names = FALSE),
      stats::quantile(x3$phi, probs = 0.005, names = FALSE)
    ),
    upper_01 = c(
      stats::quantile(x3$psi[, 1], probs = 0.995, names = FALSE),
      stats::quantile(x3$psi[, 2], probs = 0.995, names = FALSE),
      stats::quantile(x3$phi, probs = 0.995, names = FALSE)
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
