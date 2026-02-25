# Shared fixtures ---------------------------------------------------------

make_model <- function(strata_col = "age") {
  linelist <- data.frame(
    patient_id = letters,
    week = rep_len(1L:3L, 26L),
    age = rep_len(c("Female", "Male"), 26L),
    testing_type = rep_len(c("A", "A", "A", "P", "P"), 26L),
    patient_status = rep_len(c("A", "D", "S", "S"), 26L)
  )
  population <- data.frame(
    age = c("Female", "Male"),
    value = c(4000L, 3975L)
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
    )
  if (!is.null(strata_col)) {
    model <- model |> set_strata(strata_col, degrees_of_freedom = 1L)
  }
  model
}

# Stan args for a minimal fast run
STAN_ARGS <- list(
  chains = 1L,
  iter = 100L,
  seed = 1L,
  cores = 1L,
  open_progress = FALSE,
  refresh = 0L
)


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

test_that("`fit()` errors when any strata is ordered", {
  model <- make_model(strata_col = NULL) |>
    set_strata(
      "age",
      ordered = TRUE,
      levels = c("Female", "Male"),
      degrees_of_freedom = 1L
    )
  expect_error(
    fit(model),
    regexp = "Ordered strata are not yet supported",
    fixed = TRUE
  )
})


# Template compilation ----------------------------------------------------

test_that("Stan template compiles with no strata", {
  output <- stan_model(
    "estimate_severity.stan.j2",
    template_data = list(strata = list(), n_strata_dims = 0L),
    fun = rstan::stanc,
    model_name = "estimate_severity_no_strata",
    verbose = FALSE
  )
  expect_true(output$status)
})

test_that("Stan template compiles with one strata dimension", {
  output <- stan_model(
    "estimate_severity.stan.j2",
    template_data = list(
      strata = list(list(name = "age", n_levels = 3L)),
      n_strata_dims = 1L
    ),
    fun = rstan::stanc,
    model_name = "estimate_severity_one_strata",
    verbose = FALSE
  )
  expect_true(output$status)
})

test_that("Stan template compiles with two strata dimensions", {
  output <- stan_model(
    "estimate_severity.stan.j2",
    template_data = list(
      strata = list(
        list(name = "age", n_levels = 3L),
        list(name = "region", n_levels = 2L)
      ),
      n_strata_dims = 2L
    ),
    fun = rstan::stanc,
    model_name = "estimate_severity_two_strata",
    verbose = FALSE
  )
  expect_true(output$status)
})


# Output structure --------------------------------------------------------

test_that("`fit()` returns a SeverityEstimateFit with correct structure (no strata)", {
  skip_on_cran()
  model <- make_model(strata_col = NULL)
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), STAN_ARGS))
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
  model <- make_model(strata_col = "age")
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), STAN_ARGS))
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
    set_strata("sex", degrees_of_freedom = 1L) |>
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
    do.call(fit, c(list(model = model), STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  expect_s4_class(result@model_fit, "stanfit")
})

test_that("`fit()` with two strata dimensions returns correct array dimensions", {
  skip_on_cran()
  linelist <- data.frame(
    patient_id = letters,
    week = rep_len(1L:3L, 26L),
    age = rep_len(c("Female", "Male"), 26L),
    region = rep_len(c("North", "South"), 26L),
    testing_type = rep_len(c("A", "A", "A", "P", "P"), 26L),
    patient_status = rep_len(c("A", "D", "S", "S"), 26L)
  )
  population <- data.frame(
    age = rep(c("Female", "Male"), each = 2L),
    region = rep(c("North", "South"), times = 2L),
    value = c(2000L, 2000L, 2000L, 1975L)
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
    set_strata("age", degrees_of_freedom = 1L) |>
    set_strata("region", degrees_of_freedom = 1L)
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), STAN_ARGS))
  )
  expect_s4_class(result, "SeverityEstimateFit")
  # 2 age levels x 2 region levels = 4 strata cells
  expect_length(result@population, 4L)
  expect_equal(dim(result@incidence)[2L], 4L)
  expect_equal(nrow(result@strata), 4L)
  expect_setequal(names(result@strata), c("age", "region"))
})
