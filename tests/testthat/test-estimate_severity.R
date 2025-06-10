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
    "`linelist` is not 'data.frame' like.",
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
    "`linelist` is not 'data.frame' like.",
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
    "`population` is not 'data.frame' like.",
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
    "`population` is not 'data.frame' like.",
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
    regexp = paste0(
      "Assertion on 'surveillance' failed: ",
      "Must be of type 'string', not 'integer'."
    ),
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
    "Assertion on 'surveillance' failed: Must have length 1.",
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
    "Assertion on 'surveillance' failed: May not be NA.",
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
    regexp = paste0(
      "`linelist` is missing required string columns: not present in linelist."
    ),
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "week",
      "patient_status",
      time_period = "week",
      strata = "sex"
    ),
    paste0(
      "The 'week' column of `linelist` is not a character or factor, ",
      "instead is: integer."
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
    "Assertion on 'outcome' failed: Must be of type 'string', not 'integer'.",
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
    "Assertion on 'outcome' failed: Must have length 1.",
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
    "Assertion on 'outcome' failed: May not be NA.",
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
    regexp = paste0(
      "`linelist` is missing required string columns: not present in linelist."
    ),
    fixed = TRUE
  )
  expect_error(
    estimate_severity(
      linelist,
      population,
      "testing_type",
      "week",
      time_period = "week",
      strata = "sex"
    ),
    paste0(
      "The 'week' column of `linelist` is not a character or factor, ",
      "instead is: integer."
    ),
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
    paste0(
      "Assertion on 'hazard_std' failed: ",
      "Must be of type 'number', not 'character'."
    ),
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
    "Assertion on 'hazard_std' failed: Must have length 1.",
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
    "Assertion on 'hazard_std' failed: May not be NA.",
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
    "Assertion on 'hazard_std' failed: Element 1 is not >= 2.22045e-16.",
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
    paste0(
      "Assertion on 'degrees_of_freedom' failed: ",
      "Must be of type 'integerish', not 'character'."
    ),
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
    paste0(
      "Assertion on 'degrees_of_freedom' failed: ",
      "Must be of type 'integerish', ",
      "but element 1 is not close to an integer."
    ),
    fixed = TRUE
  )
})

test_that("Output Validation", {
  # Slow, skip if not manual run
  skip_on_cran()
  # Sample data
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
  # Expectations on output structure
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
  # Expectations on output structure
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
