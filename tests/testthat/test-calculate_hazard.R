test_that("Non-SeverityEstimateFit or list object given for `x`", {
  lapply(list(NULL, data.frame(), Sys.time()), function(x) {
    expect_error(
      calculate_hazard(x),
      regexp = paste0(
        "Unable to find a suitable `calculate_hazard` method for ",
        "`x` with classes: ",
        toString(class(x)),
        "."
      ),
      fixed = TRUE
    )
  })
})

test_that("Input validation when given a list for `x`", {
  valid_x <- list(
    "logit_hzd" = array(
      data = rnorm(100L * 5L * 3L),
      dim = c(100L, 5L, 3L),
      dimnames = list("iterations" = NULL)
    )
  )
  time_period <- data.frame(week = seq_len(5L))
  strata <- data.frame(age_group = c("Children", "Adults", "Seniors"))

  expect_error(
    calculate_hazard(list("abc" = letters), time_period, strata),
    regexp = "Names must include the elements \\{'logit_hzd'\\}",
    perl = TRUE
  )

  lapply(list(NA, NULL, c(TRUE, TRUE), 1L), function(x) {
    expect_error(calculate_hazard(
      valid_x,
      time_period,
      strata,
      mean_estimate = x
    ))
    expect_error(
      calculate_hazard(valid_x, time_period, strata, median_estimate = x)
    )
  })

  expect_error(
    calculate_hazard(
      valid_x,
      time_period,
      strata,
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

  expect_error(calculate_hazard(valid_x, list(), strata))
  expect_error(calculate_hazard(valid_x, time_period, list()))
  expect_error(
    calculate_hazard(valid_x, data.frame(day = seq_len(4L)), strata),
    regexp = paste0(
      "The rows of `time_period` must match the second dimension of ",
      "`x\\$logit_hzd`\\."
    )
  )
  expect_error(
    calculate_hazard(valid_x, time_period, data.frame(age_group = c("A", "B"))),
    regexp = paste0(
      "The rows of `strata` must match the third dimension of ",
      "`x\\$logit_hzd`\\."
    )
  )
  expect_error(
    calculate_hazard(
      valid_x,
      time_period,
      strata,
      population = c(10L, 0L)
    ),
    regexp = "Must have length 3, but has length 2"
  )
  expect_error(
    calculate_hazard(
      valid_x,
      time_period,
      strata,
      population = c(0L, 0L, 0L)
    ),
    regexp = "At least one strata group must have a positive population.",
    fixed = TRUE
  )
})

test_that("Output validation when given a list for `x`", {
  hazard_probs <- array(
    data = c(
      0.10,
      0.30,
      0.50,
      0.20,
      0.40,
      0.60,
      0.70,
      0.80,
      0.90,
      0.15,
      0.35,
      0.55
    ),
    dim = c(3L, 2L, 2L),
    dimnames = list("iterations" = NULL)
  )
  x <- list("logit_hzd" = qlogis(hazard_probs))
  hazard_probs <- plogis(x$logit_hzd)
  hazard_draws <- aperm(hazard_probs, c(3L, 2L, 1L))
  dim(hazard_draws) <- c(4L, 3L)
  time_period <- data.frame(week = c(1L, 2L))
  strata <- data.frame(age_group = c("Children", "Adults"))

  hazard <- calculate_hazard(x, time_period, strata, alpha = numeric())
  expected_hazard <- data.frame(
    week = c(1L, 1L, 2L, 2L),
    age_group = c("Children", "Adults", "Children", "Adults"),
    mean_estimate = rowMeans(hazard_draws),
    median_estimate = c(
      median(hazard_probs[, 1L, 1L]),
      median(hazard_probs[, 1L, 2L]),
      median(hazard_probs[, 2L, 1L]),
      median(hazard_probs[, 2L, 2L])
    )
  )
  expect_identical(hazard, expected_hazard)

  hazard <- calculate_hazard(
    x,
    time_period,
    strata,
    median_estimate = FALSE,
    alpha = c(0.05, 0.01)
  )
  expected_hazard <- data.frame(
    week = c(1L, 1L, 2L, 2L),
    age_group = c("Children", "Adults", "Children", "Adults"),
    mean_estimate = rowMeans(hazard_draws),
    lower_05 = c(
      stats::quantile(hazard_probs[, 1L, 1L], probs = 0.025, names = FALSE),
      stats::quantile(hazard_probs[, 1L, 2L], probs = 0.025, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 1L], probs = 0.025, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 2L], probs = 0.025, names = FALSE)
    ),
    upper_05 = c(
      stats::quantile(hazard_probs[, 1L, 1L], probs = 0.975, names = FALSE),
      stats::quantile(hazard_probs[, 1L, 2L], probs = 0.975, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 1L], probs = 0.975, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 2L], probs = 0.975, names = FALSE)
    ),
    lower_01 = c(
      stats::quantile(hazard_probs[, 1L, 1L], probs = 0.005, names = FALSE),
      stats::quantile(hazard_probs[, 1L, 2L], probs = 0.005, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 1L], probs = 0.005, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 2L], probs = 0.005, names = FALSE)
    ),
    upper_01 = c(
      stats::quantile(hazard_probs[, 1L, 1L], probs = 0.995, names = FALSE),
      stats::quantile(hazard_probs[, 1L, 2L], probs = 0.995, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 1L], probs = 0.995, names = FALSE),
      stats::quantile(hazard_probs[, 2L, 2L], probs = 0.995, names = FALSE)
    )
  )
  expect_identical(hazard, expected_hazard)
})

test_that("Output validation excludes zero population strata", {
  hazard_probs <- array(
    data = c(
      0.10,
      0.20,
      0.30,
      0.40,
      0.50,
      0.60,
      0.70,
      0.80,
      0.90,
      0.15,
      0.25,
      0.35
    ),
    dim = c(2L, 2L, 3L),
    dimnames = list("iterations" = NULL)
  )
  x <- list("logit_hzd" = qlogis(hazard_probs))
  time_period <- data.frame(week = c(1L, 2L))
  strata <- data.frame(age_group = c("Children", "Adults", "Seniors"))
  population <- c(100L, 0L, 200L)

  hazard <- calculate_hazard(
    x,
    time_period,
    strata,
    population = population,
    mean_estimate = FALSE,
    alpha = numeric()
  )

  expected_hazard <- data.frame(
    week = c(1L, 1L, 2L, 2L),
    age_group = c("Children", "Seniors", "Children", "Seniors"),
    median_estimate = c(
      median(hazard_probs[, 1L, 1L]),
      median(hazard_probs[, 1L, 3L]),
      median(hazard_probs[, 2L, 1L]),
      median(hazard_probs[, 2L, 3L])
    )
  )
  expect_identical(
    hazard[, c("week", "age_group")],
    expected_hazard[, c(
      "week",
      "age_group"
    )]
  )
  expect_equal(hazard$median_estimate, expected_hazard$median_estimate)
})

test_that("Output validation when there are no strata columns", {
  hazard_probs <- array(
    data = c(0.10, 0.20, 0.80, 0.90),
    dim = c(2L, 2L, 1L),
    dimnames = list("iterations" = NULL)
  )
  x <- list("logit_hzd" = qlogis(hazard_probs))
  hazard_probs <- plogis(x$logit_hzd)
  time_period <- data.frame(week = c(1L, 2L))
  strata <- data.frame(.strata = 1L)[, character(0L), drop = FALSE]

  hazard <- calculate_hazard(
    x,
    time_period,
    strata,
    mean_estimate = FALSE,
    alpha = numeric()
  )

  expect_identical(
    hazard,
    data.frame(
      week = c(1L, 2L),
      median_estimate = c(
        median(hazard_probs[, 1L, 1L]),
        median(hazard_probs[, 2L, 1L])
      )
    )
  )
})
