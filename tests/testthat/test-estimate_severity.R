test_that("Input Validation", {
  # Ideal valid inputs
  linelist <- data.frame(
    patient_id = letters,
    week = rep_len(1L:3L, 26L),
    sex = rep_len(c("Female", "Male"), 26L),
    testing_type = rep_len(c("A", "A", "A", "P", "P"), 26L),
    patient_status = rep_len(c("A", "D", "S", "S"), 26L)
  )
  population <- data.frame(
    sex = c("Female", "Male"),
    value = c(4000L, 3975L)
  )

  # linelist param
  expect_error(
    estimate_severity(
      NULL,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "is.data.frame(linelist) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      list(abc = letters, def = LETTERS),
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "is.data.frame(linelist) is not TRUE",
    fixed = TRUE
  )

  # population param
  expect_error(
    estimate_severity(
      linelist,
      NULL,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "is.data.frame(population) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      list(abc = letters, def = LETTERS),
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "is.data.frame(population) is not TRUE",
    fixed = TRUE
  )

  # surveillance param
  expect_error(
    estimate_severity(
      linelist,
      population,
      1L,
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "is.character(surveillance) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      letters,
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "length(surveillance) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      NA_character_,
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "!is.na(surveillance) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "not present in linelist",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    "surveillance %in% names(linelist) is not TRUE",
    fixed = TRUE
  )
  expect_error( # fails for not being a character/factor
    estimate_severity(
      linelist,
      population,
      "week",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    paste0(
      "is.factor(linelist[, surveillance]) || is.character(linelist[,  ",
      ".... is not TRUE"
    ),
    fixed = TRUE
  )

  # outcome param
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      1L,
      time_period = "week",
      strata = "sex"
    ),
    "is.character(outcome) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      letters,
      time_period = "week",
      strata = "sex"
    ),
    "length(outcome) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      NA_character_,
      time_period = "week",
      strata = "sex"
    ),
    "!is.na(outcome) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "not present in linelist",
      time_period = "week",
      strata = "sex"
    ),
    "outcome %in% names(linelist) is not TRUE",
    fixed = TRUE
  )
  expect_error( # fails for not being a character/factor
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "week",
      time_period = "week",
      strata = "sex"
    ),
    paste0(
      "is.factor(linelist[, outcome]) || is.character(linelist[, outcome]) ",
      "is not TRUE"
    ),
    fixed = TRUE
  )

  # additional_betas_mean param
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_mean = "abc"
    ),
    "is.numeric(additional_betas_mean) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_mean = 1:5
    ),
    "length(additional_betas_mean) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_mean = NA_real_
    ),
    "!is.na(additional_betas_mean) is not TRUE",
    fixed = TRUE
  )

  # additional_betas_std param
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = "abc"
    ),
    "is.numeric(additional_betas_std) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = 1:5
    ),
    "length(additional_betas_std) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = NA_real_
    ),
    "!is.na(additional_betas_std) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = -pi
    ),
    "additional_betas_std > 0 is not TRUE",
    fixed = TRUE
  )

  # hazard_std param
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      hazard_std = "abc"
    ),
    "is.numeric(hazard_std) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      hazard_std = 1:5
    ),
    "length(hazard_std) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      hazard_std = NA_real_
    ),
    "!is.na(hazard_std) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      hazard_std = -pi
    ),
    "hazard_std > 0 is not TRUE",
    fixed = TRUE
  )

  # degrees_of_freedom param
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      degrees_of_freedom = "abc"
    ),
    "is.integer(degrees_of_freedom) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      degrees_of_freedom = pi
    ),
    "is.integer(degrees_of_freedom) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = 1L:5L
    ),
    "length(additional_betas_std) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = NA_integer_
    ),
    "!is.na(additional_betas_std) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      additional_betas_std = -1L
    ),
    "additional_betas_std > 0 is not TRUE",
    fixed = TRUE
  )
})

test_that("Output Validation", {
  # Slow, skip if not manual run
  skip_on_cran()

  linelist <- data.frame(
    patient_id = letters,
    week = rep_len(1L:3L, 26L),
    sex = rep_len(c("Female", "Male"), 26L),
    testing_type = rep_len(c("A", "A", "A", "P", "P"), 26L),
    patient_status = rep_len(c("A", "D", "S", "S"), 26L)
  )
  population <- data.frame(
    sex = c("Female", "Male"),
    value = c(4000L, 3975L)
  )

  # Categories inferred from linelist/population
  severity_est <- suppressWarnings(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      # Params passed to rstan::stan
      chains = 1L,
      iter = 100L,
      seed = 1L,
      cores = 1L,
      open_progress = FALSE,
      refresh = 0L
    )
  )

  expect_s4_class(severity_est, "SeverityEstimateFit")
  expect_equal(
    slotNames(severity_est),
    c(
      "model_fit", "population", "incidence", "time_period",
      "strata", "surveillance", "outcome"
    )
  )
  expect_s4_class(severity_est@model_fit, "stanfit")
  expected_population <- array(
    data = c(4000L, 3975L),
    dimnames = list("strata" = seq_len(2))
  )
  expect_identical(severity_est@population, expected_population)
  expected_incidence <- array(
    data = c(
      2L, 1L, 1L,
      0L, 0L, 0L,
      1L, 1L, 1L,
      0L, 0L, 0L,
      0L, 0L, 0L,
      1L, 2L, 2L,
      0L, 0L, 0L,
      1L, 1L, 0L,
      1L, 2L, 1L,
      1L, 1L, 1L,
      1L, 0L, 1L,
      1L, 1L, 1L
    ),
    dim = c(3L, 2L, 2L, 3L),
    dimnames = list(
      "time_period" = seq_len(3L),
      "strata" = seq_len(2L),
      "surveillance" = seq_len(2L),
      "outcome" = seq_len(3L)
    )
  )
  expect_identical(severity_est@incidence, expected_incidence)
  expected_time_period <- data.frame(week = 1L:3L)
  expect_identical(severity_est@time_period, expected_time_period)
  expected_strata <- data.frame(sex = c("Female", "Male"))
  expect_identical(severity_est@strata, expected_strata)
  expected_surveillance <- data.frame(
    testing_type = c("Active", "Passive"),
    stringsAsFactors = TRUE
  )
  expect_identical(severity_est@surveillance, expected_surveillance)
  expected_outcome <- data.frame(
    patient_status = c("Asymptomatic", "Death", "Symptomatic"),
    stringsAsFactors = TRUE
  )
  expect_identical(severity_est@outcome, expected_outcome)

  # Time period/strata categories are user specified
  severity_est <- suppressMessages(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "patient_status",
      time_period = "week",
      strata = "sex",
      time_period_reference = 0L:4L,
      strata_reference = c("Female", "Male", "NA"),
      # Params passed to rstan::stan
      chains = 1L,
      iter = 100L,
      seed = 1L,
      cores = 1L,
      open_progress = FALSE,
      refresh = 0L
    )
  )

  expect_s4_class(severity_est, "SeverityEstimateFit")
  expect_equal(
    slotNames(severity_est),
    c(
      "model_fit", "population", "incidence", "time_period",
      "strata", "surveillance", "outcome"
    )
  )
  expect_s4_class(severity_est@model_fit, "stanfit")
  expected_population <- array(
    data = c(4000L, 3975L, 0L),
    dimnames = list("strata" = seq_len(3L))
  )
  expect_identical(severity_est@population, expected_population)
  expected_incidence <- array(
    data = c(
      0L, 2L, 1L, 1L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 1L, 1L, 1L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 1L, 2L, 2L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 1L, 1L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 1L, 2L, 1L, 0L,
      0L, 1L, 1L, 1L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 1L, 0L, 1L, 0L,
      0L, 1L, 1L, 1L, 0L,
      0L, 0L, 0L, 0L, 0L
    ),
    dim = c(5L, 3L, 2L, 3L),
    dimnames = list(
      "time_period" = seq_len(5L),
      "strata" = seq_len(3L),
      "surveillance" = seq_len(2L),
      "outcome" = seq_len(3L)
    )
  )
  expect_identical(severity_est@incidence, expected_incidence)
  expected_time_period <- data.frame(week = 0L:4L)
  expect_identical(severity_est@time_period, expected_time_period)
  expected_strata <- data.frame(sex = c("Female", "Male", "NA"))
  expect_identical(severity_est@strata, expected_strata)
  expected_surveillance <- data.frame(
    testing_type = c("Active", "Passive"),
    stringsAsFactors = TRUE
  )
  expect_identical(severity_est@surveillance, expected_surveillance)
  expected_outcome <- data.frame(
    patient_status = c("Asymptomatic", "Death", "Symptomatic"),
    stringsAsFactors = TRUE
  )
  expect_identical(severity_est@outcome, expected_outcome)
})
