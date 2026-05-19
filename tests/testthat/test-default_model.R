test_that("`default_model()` infers structure and priors from formatted data", {
  line_list <- data.frame(
    time = c(1L, 1L, 2L, 2L),
    age = c("Youth", "Adult", "Youth", "Adult"),
    region = c("North", "North", "South", "South"),
    detection = c("Active", "Passive", "Active", "Passive"),
    outcome = c("Asymptomatic", "Death", "Symptomatic", "Asymptomatic")
  )
  population <- data.frame(
    value = c(100L, 120L, 140L, 160L),
    age = c("Adult", "Adult", "Youth", "Youth"),
    region = c("North", "South", "North", "South")
  )

  model <- default_model(line_list, population)

  expect_s4_class(model, "SeverityEstimateModel")
  expect_named(model@population, c("age", "region", "value"))
  expect_identical(
    timesteps(model),
    list(name = "time", levels = 1L:2L)
  )
  expect_identical(
    detection(model),
    list(
      name = "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  )
  expect_identical(
    outcome(model),
    list(
      name = "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Death" = "severe",
        "Symptomatic" = "symptomatic"
      )
    )
  )
  expect_identical(
    strata(model),
    list(
      list(
        name = "age",
        levels = c("Adult", "Youth"),
        degrees_of_freedom = 0L
      ),
      list(
        name = "region",
        levels = c("North", "South"),
        degrees_of_freedom = 0L
      )
    )
  )
  expect_identical(active_prior(model), c(alpha = 1.0, beta = 1.0))
  expect_identical(
    passive_asymptomatic_prior(model),
    c(alpha = 1.0, beta = 3.0)
  )
  expect_identical(
    passive_symptomatic_prior(model),
    c(alpha = 3.0, beta = 1.0)
  )
})

test_that("`default_model()` returns a model ready to `fit()`", {
  skip_on_cran()
  line_list <- data.frame(
    time = rep_len(1L:3L, 18L),
    sex = rep_len(c("Female", "Male"), 18L),
    detection = rep_len(c("A", "A", "P"), 18L),
    outcome = rep_len(c("A", "S", "D"), 18L)
  )
  population <- data.frame(
    sex = c("Female", "Male"),
    value = c(4000L, 3975L)
  )

  model <- default_model(line_list, population)
  result <- suppressWarnings(
    do.call(fit, c(list(model = model), FIT_TEST_STAN_ARGS))
  )

  expect_s4_class(result, "SeverityEstimateFit")
  expect_identical(result@strata$sex, c("Female", "Male"))
})

test_that("`default_model()` validates inferred schema", {
  expect_error(
    default_model(
      data.frame(
        time = 1L,
        outcome = "Asymptomatic",
        age = "Youth"
      ),
      data.frame(age = "Youth", value = 100L)
    ),
    regexp = "`line_list` is missing required string columns: detection.",
    fixed = TRUE
  )

  expect_error(
    default_model(
      data.frame(
        time = 1L,
        age = "Youth",
        detection = "Active",
        outcome = "Asymptomatic"
      ),
      data.frame(value = 100L)
    ),
    regexp = "`population` is missing inferred strata columns: age.",
    fixed = TRUE
  )

  expect_error(
    default_model(
      data.frame(
        time = c(1L, 1L),
        detection = c("Active", "Passive"),
        outcome = c("Asymptomatic", "Death")
      ),
      data.frame(value = 100L, total = 100L)
    ),
    regexp = paste0(
      "`population` must contain exactly one non-strata column ",
      "representing the population counts. Found: value, total."
    ),
    fixed = TRUE
  )

  expect_error(
    default_model(
      data.frame(
        time = c(1L, 1L),
        detection = c("screening", "Passive"),
        outcome = c("Asymptomatic", "Death")
      ),
      data.frame(value = 100L)
    ),
    regexp = paste0(
      "The `detection` column contains values that `default_model()` ",
      "cannot map: screening."
    ),
    fixed = TRUE
  )
})
