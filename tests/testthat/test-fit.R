# Validation errors -------------------------------------------------------

test_that("`fit()` errors when timesteps not set", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_error(
    fit(model),
    regexp = "No timesteps have been set.",
    fixed = TRUE
  )
})

test_that("`fit()` errors when detection not set", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
    set_timesteps("week")
  expect_error(
    fit(model),
    regexp = "No detection mapping has been set.",
    fixed = TRUE
  )
})

test_that("`fit()` errors when outcome not set", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
    set_timesteps("week") |>
    set_detection(
      "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  expect_error(
    fit(model),
    regexp = "No outcome mapping has been set.",
    fixed = TRUE
  )
})


# Stan compilation --------------------------------------------------------

test_that("Generic severity Stan model compiles", {
  output <- rstan::stanc(
    file = system.file(
      "stan",
      "estimate_severity.stan",
      package = "SeverityEstimate"
    ),
    model_name = "estimate_severity",
    verbose = FALSE
  )
  expect_true(output$status)
})


# Output structure --------------------------------------------------------

test_that("`fit()` returns a SeverityEstimateFit with correct structure (no strata)", {
  skip_on_cran()
  model <- MAKE_FIT_TEST_MODEL(strata_col = NULL)
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), FIT_TEST_STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  expect_equal(
    slotNames(result),
    c(
      "model_fit",
      "population",
      "incidence",
      "time_period",
      "strata",
      "surveillance",
      "outcome"
    )
  )
  expect_s4_class(result@model_fit, "stanfit")
  # No strata: single cell
  expect_length(result@population, 1L)
  expect_equal(dim(result@incidence)[1L], 3L) # 3 weeks
  expect_equal(dim(result@incidence)[2L], 1L) # 1 strata cell
  expect_equal(dim(result@incidence)[3L], 2L) # active + passive
  expect_equal(dim(result@incidence)[4L], 3L) # asymptomatic/symptomatic/death
  expect_equal(nrow(result@time_period), 3L)
  expect_equal(nrow(result@strata), 1L)
  expect_equal(nrow(result@surveillance), 2L)
  expect_equal(nrow(result@outcome), 3L)
})

test_that("`fit()` returns a SeverityEstimateFit with correct structure (with strata)", {
  skip_on_cran()
  model <- MAKE_FIT_TEST_MODEL(strata_col = "age")
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), FIT_TEST_STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  expect_s4_class(result@model_fit, "stanfit")
  # With strata: Female + Male
  expect_length(result@population, 2L)
  expect_equal(dim(result@incidence)[1L], 3L) # 3 weeks
  expect_equal(dim(result@incidence)[2L], 2L) # 2 strata cells
  expect_equal(dim(result@incidence)[3L], 2L) # active + passive
  expect_equal(dim(result@incidence)[4L], 3L) # asymptomatic/symptomatic/death
  expect_equal(nrow(result@time_period), 3L)
  expect_equal(nrow(result@strata), 2L)
  expect_equal(result@strata$age, c("Female", "Male"))
  expect_equal(
    result@population,
    array(c(4000L, 3975L), dimnames = list("strata" = seq_len(2L)))
  )
  expected_surveillance <- data.frame(
    testing_type = factor(
      c("Active", "Passive"),
      levels = c("Active", "Passive")
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(result@surveillance, expected_surveillance)
  expected_outcome <- data.frame(
    patient_status = factor(
      c("Asymptomatic", "Death", "Symptomatic"),
      levels = c("Asymptomatic", "Death", "Symptomatic")
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(result@outcome, expected_outcome)
})

test_that("`fit()` handles a single passive observation with strata", {
  skip_on_cran()
  line_list <- data.frame(
    id = 1L:3L,
    week = c(1L, 1L, 2L),
    sex = c("M", "F", "M"),
    outcome = c("Asymptomatic", "Symptomatic", "Death"),
    detection = c("Active", "Active", "Passive")
  )
  population <- data.frame(
    sex = c("M", "F"),
    amount = c(123L, 456L)
  )
  model <- SeverityEstimateModel(line_list, population) |>
    set_active_prior(alpha = 1.0, beta = 1.0) |>
    set_passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
    set_passive_symptomatic_prior(alpha = 3.0, beta = 1.0) |>
    set_strata("sex", degrees_of_freedom = 0L) |>
    set_timesteps("week") |>
    set_detection(
      "detection",
      map = c("Active" = "active", "Passive" = "passive")
    ) |>
    set_outcome(
      "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), FIT_TEST_STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  expect_s4_class(result@model_fit, "stanfit")
})

test_that("`fit()` supports a smoothed ordered strata dimension", {
  skip_on_cran()
  line_list <- data.frame(
    patient_id = 1L:12L,
    week = rep(1L:3L, each = 4L),
    age = rep(c("Youth", "Adult", "Senior"), each = 4L),
    testing_type = rep(c("A", "A", "P", "P"), 3L),
    patient_status = rep(c("A", "S", "S", "D"), 3L)
  )
  population <- data.frame(
    age = c("Youth", "Adult", "Senior"),
    value = c(1200L, 1800L, 900L)
  )
  model <- SeverityEstimateModel(line_list, population) |>
    set_timesteps("week") |>
    set_detection(
      "testing_type",
      map = c("A" = "active", "P" = "passive")
    ) |>
    set_outcome(
      "patient_status",
      map = c("A" = "asymptomatic", "S" = "symptomatic", "D" = "severe")
    ) |>
    set_strata(
      "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    )
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), FIT_TEST_STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  expect_s4_class(result@model_fit, "stanfit")
  expect_identical(result@strata$age, c("Youth", "Adult", "Senior"))
})

test_that("`fit()` supports mixed smoothed and categorical strata", {
  skip_on_cran()
  linelist <- data.frame(
    patient_id = 1L:18L,
    week = rep_len(1L:3L, 18L),
    age = rep_len(c("Youth", "Adult", "Senior"), 18L),
    region = rep_len(c("North", "South"), 18L),
    testing_type = rep_len(c("A", "A", "P"), 18L),
    patient_status = rep_len(c("A", "S", "D"), 18L)
  )
  population <- data.frame(
    age = rep(c("Youth", "Adult", "Senior"), each = 2L),
    region = rep(c("North", "South"), times = 3L),
    value = c(800L, 700L, 900L, 850L, 600L, 650L)
  )
  model <- SeverityEstimateModel(linelist, population) |>
    set_timesteps("week") |>
    set_detection(
      "testing_type",
      map = c("A" = "active", "P" = "passive")
    ) |>
    set_outcome(
      "patient_status",
      map = c("A" = "asymptomatic", "S" = "symptomatic", "D" = "severe")
    ) |>
    set_strata(
      "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    ) |>
    set_strata("region", degrees_of_freedom = 0L)
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), FIT_TEST_STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  # 3 age levels x 2 region levels = 6 strata cells
  expect_length(result@population, 6L)
  expect_equal(dim(result@incidence)[2L], 6L)
  expect_equal(nrow(result@strata), 6L)
  expect_identical(
    result@strata,
    data.frame(
      age = rep(c("Youth", "Adult", "Senior"), each = 2L),
      region = rep(c("North", "South"), times = 3L)
    )
  )
  expect_identical(
    result@population,
    array(
      c(800L, 700L, 900L, 850L, 600L, 650L),
      dimnames = list("strata" = seq_len(6L))
    )
  )
})
