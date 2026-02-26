test_that("`prior()` is not exported", {
  namespace_lines <- readLines(testthat::test_path("../../NAMESPACE"))
  expect_false(any(grepl("^export\\(prior\\)$", namespace_lines)))
})

test_that("`active_prior()` getter warns and returns a default if unset", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_warning(
    prior_value <- active_prior(model),
    regexp = paste0(
      "^The given 'model' has no active prior set\\. Returning the default ",
      "uniformative prior alpha = 1, beta = 1\\.$"
    )
  )
  expect_identical(prior_value, c(alpha = 1.0, beta = 1.0))
})

test_that("`active_prior()` getter returns the active prior slot when set", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  model <- set_active_prior(model, alpha = 1.0, beta = 2.0)
  expect_identical(active_prior(model), model@active_prior)
})

test_that("`active_prior<-` replacement setter updates the active prior slot", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_length(model@active_prior, 0L)

  active_prior(model) <- list(alpha = 1.0, beta = 2.0)
  expect_type(model@active_prior, "double")
  expect_length(model@active_prior, 2L)
  expect_setequal(names(model@active_prior), c("alpha", "beta"))

  expect_warning(
    active_prior(model) <- c(mean = 0.5, sd = 0.1),
    regexp = paste0(
      "^The given 'model' has an attribute called 'active_prior' which has ",
      "already been set\\. The previously set value will be overridden\\.$"
    )
  )
  expect_setequal(names(model@active_prior), c("alpha", "beta"))
})

test_that("`set_active_prior()` supports chainable named parameter usage", {
  prior_value <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
    set_active_prior(alpha = 10.0, beta = 2.0) |>
    active_prior()

  expect_type(prior_value, "double")
  expect_length(prior_value, 2L)
  expect_setequal(names(prior_value), c("alpha", "beta"))
})

test_that("`passive_asymptomatic_prior()` getter warns and returns a default if unset", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_warning(
    prior_value <- passive_asymptomatic_prior(model),
    regexp = paste0(
      "^The given 'model' has no passive asymptomatic prior set\\. Returning ",
      "the default uniformative prior alpha = 1, beta = 1\\.$"
    )
  )
  expect_identical(prior_value, c(alpha = 1.0, beta = 1.0))
})

test_that("`passive_asymptomatic_prior()` getter returns the slot when set", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  model <- set_passive_asymptomatic_prior(model, alpha = 1.0, beta = 2.0)
  expect_identical(
    passive_asymptomatic_prior(model),
    model@passive_asymptomatic_prior
  )
})

test_that("`passive_asymptomatic_prior<-` replacement setter updates the slot", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_length(model@passive_asymptomatic_prior, 0L)

  passive_asymptomatic_prior(model) <- list(alpha = 1.0, beta = 2.0)
  expect_type(model@passive_asymptomatic_prior, "double")
  expect_length(model@passive_asymptomatic_prior, 2L)
  expect_setequal(names(model@passive_asymptomatic_prior), c("alpha", "beta"))

  expect_warning(
    passive_asymptomatic_prior(model) <- c(mean = 0.5, sd = 0.1),
    regexp = paste0(
      "^The given 'model' has an attribute called 'passive_asymptomatic_prior' ",
      "which has already been set\\. The previously set value will be ",
      "overridden\\.$"
    )
  )
  expect_setequal(names(model@passive_asymptomatic_prior), c("alpha", "beta"))
})

test_that("`set_passive_asymptomatic_prior()` supports chainable piping", {
  prior_value <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
    set_passive_asymptomatic_prior(alpha = 10.0, beta = 2.0) |>
    passive_asymptomatic_prior()

  expect_type(prior_value, "double")
  expect_length(prior_value, 2L)
  expect_setequal(names(prior_value), c("alpha", "beta"))
})

test_that("`passive_symptomatic_prior()` getter warns and returns a default if unset", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_warning(
    prior_value <- passive_symptomatic_prior(model),
    regexp = paste0(
      "^The given 'model' has no passive symptomatic prior set\\. Returning ",
      "the default uniformative prior alpha = 1, beta = 1\\.$"
    )
  )
  expect_identical(prior_value, c(alpha = 1.0, beta = 1.0))
})

test_that("`passive_symptomatic_prior()` getter returns the slot when set", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  model <- set_passive_symptomatic_prior(model, alpha = 1.0, beta = 2.0)
  expect_identical(
    passive_symptomatic_prior(model),
    model@passive_symptomatic_prior
  )
})

test_that("`passive_symptomatic_prior<-` replacement setter updates the slot", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_length(model@passive_symptomatic_prior, 0L)

  passive_symptomatic_prior(model) <- list(alpha = 1.0, beta = 2.0)
  expect_type(model@passive_symptomatic_prior, "double")
  expect_length(model@passive_symptomatic_prior, 2L)
  expect_setequal(names(model@passive_symptomatic_prior), c("alpha", "beta"))

  expect_warning(
    passive_symptomatic_prior(model) <- c(mean = 0.5, sd = 0.1),
    regexp = paste0(
      "^The given 'model' has an attribute called 'passive_symptomatic_prior' ",
      "which has already been set\\. The previously set value will be ",
      "overridden\\.$"
    )
  )
  expect_setequal(names(model@passive_symptomatic_prior), c("alpha", "beta"))
})

test_that("`set_passive_symptomatic_prior()` supports chainable piping", {
  prior_value <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
    set_passive_symptomatic_prior(alpha = 10.0, beta = 2.0) |>
    passive_symptomatic_prior()

  expect_type(prior_value, "double")
  expect_length(prior_value, 2L)
  expect_setequal(names(prior_value), c("alpha", "beta"))
})

test_that("Public prior APIs provide output validation across parameters", {
  replacement_setters <- list(
    "active" = function(model, value) {
      active_prior(model) <- value
      model
    },
    "passive_asymptomatic" = function(model, value) {
      passive_asymptomatic_prior(model) <- value
      model
    },
    "passive_symptomatic" = function(model, value) {
      passive_symptomatic_prior(model) <- value
      model
    }
  )

  for (parameter in names(replacement_setters)) {
    parameter_prior <- paste0(parameter, "_prior")
    for (params in list(
      list(alpha = 1.0, beta = 2.0),
      list(mean = 0.5, sd = 0.1),
      list(mean = 0.3, var = 0.05)
    )) {
      model <- SeverityEstimateModel(LINE_LIST, POPULATION)
      expect_length(methods::slot(model, parameter_prior), 0L)
      model <- replacement_setters[[parameter]](model, params)
      expect_silent(first_slot_value <- methods::slot(model, parameter_prior))
      expect_type(first_slot_value, "double")
      expect_length(first_slot_value, 2L)
      expect_setequal(names(first_slot_value), c("alpha", "beta"))
      expect_warning(
        model <- replacement_setters[[parameter]](
          model,
          Map(\(x) 0.5 * x, params)
        ),
        regexp = paste0(
          "^The given 'model' has an attribute called '",
          parameter_prior,
          "' which has already been set\\. The previously set value will be ",
          "overridden\\.$"
        )
      )
      second_slot_value <- methods::slot(model, parameter_prior)
      expect_type(first_slot_value, "double")
      expect_length(first_slot_value, 2L)
      expect_setequal(names(first_slot_value), c("alpha", "beta"))
      expect_false(isTRUE(all.equal(first_slot_value, second_slot_value)))
    }
  }
})
