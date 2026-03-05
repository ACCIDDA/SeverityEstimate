test_that("`outcome()` getter returns the outcome slot", {
  expect_identical(outcome(MODEL), list())
  model <- MODEL |>
    set_outcome(
      "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  expect_identical(
    outcome(model),
    list(
      name = "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  )
})

test_that("`set_outcome()` default map works for lowercase values", {
  line_list_lower <- data.frame(
    patient = 1L:3L,
    week = c(1L, 1L, 2L),
    outcome = c("asymptomatic", "symptomatic", "severe")
  )
  model_lower <- SeverityEstimateModel(line_list_lower, 1000L)
  expect_identical(outcome(model_lower), list())
  model <- model_lower |> set_outcome("outcome")
  expect_identical(
    outcome(model),
    list(
      name = "outcome",
      map = c(
        "asymptomatic" = "asymptomatic",
        "symptomatic" = "symptomatic",
        "severe" = "severe"
      )
    )
  )
})

test_that("`outcome<-` replacement setter updates and can override", {
  expect_identical(outcome(MODEL), list())
  model <- MODEL
  outcome(model) <- list(
    name = "outcome",
    map = c(
      "Asymptomatic" = "asymptomatic",
      "Symptomatic" = "symptomatic",
      "Death" = "severe"
    )
  )
  expect_identical(
    outcome(model),
    list(
      name = "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  )
  expect_warning(
    outcome(model) <- list(
      name = "outcome",
      map = c(
        "Asymptomatic" = "symptomatic",
        "Symptomatic" = "severe",
        "Death" = "asymptomatic"
      )
    ),
    regexp = paste0(
      "The given 'model' has outcome already set to 'outcome'. ",
      "The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    outcome(model),
    list(
      name = "outcome",
      map = c(
        "Asymptomatic" = "symptomatic",
        "Symptomatic" = "severe",
        "Death" = "asymptomatic"
      )
    )
  )
})

test_that("Using outcome with invalid map values raises an error", {
  expect_error(
    MODEL |> set_outcome("outcome", map = c("Asymptomatic" = "invalid")),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be ",
      "one of 'asymptomatic', 'symptomatic', 'severe'"
    ),
    fixed = TRUE
  )
  expect_error(
    MODEL |>
      set_outcome(
        "outcome",
        map = c(
          "Asymptomatic" = "asymptomatic",
          "Symptomatic" = "other"
        )
      ),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be ",
      "one of 'asymptomatic', 'symptomatic', 'severe'"
    ),
    fixed = TRUE
  )
})

test_that("Using outcome with map names not in column raises an error", {
  expect_error(
    MODEL |> set_outcome("outcome", map = c("NotPresent" = "asymptomatic")),
    regexp = "Must be a subset of \\{'Asymptomatic','Death','Symptomatic'\\}",
    fixed = FALSE
  )
  expect_error(
    MODEL |>
      set_outcome(
        "outcome",
        map = c(
          "Asymptomatic" = "asymptomatic",
          "NonExistent" = "symptomatic"
        )
      ),
    regexp = "Must be a subset of \\{'Asymptomatic','Death','Symptomatic'\\}",
    fixed = FALSE
  )
})

test_that("Using outcome with invalid column name raises an error", {
  expect_error(
    MODEL |> set_outcome("nonexistent_column"),
    regexp = "Must be element of set \\{.*\\}",
    fixed = FALSE
  )
})

test_that("`has_outcome()` returns whether outcome is set", {
  expect_false(has_outcome(MODEL))
  model <- MODEL |>
    set_outcome(
      "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  expect_true(has_outcome(model))
})

test_that("`require_outcome()` checks presence according to mode", {
  expect_error(
    require_outcome(MODEL),
    regexp = "No outcome mapping has been set. Call `set_outcome\\(\\.\\.\\.\\)` first\\."
  )
  expect_warning(
    require_outcome(MODEL, mode = "warn"),
    regexp = "No outcome mapping has been set. Call `set_outcome\\(\\.\\.\\.\\)` first\\."
  )
  expect_identical(require_outcome(MODEL, mode = "silent"), MODEL)

  model <- MODEL |>
    set_outcome(
      "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  expect_identical(require_outcome(model), model)
})

test_that("`require_outcome()` validates mode", {
  for (mode in c("nope", "loud", "", "warning")) {
    expect_error(
      require_outcome(MODEL, mode = mode),
      regexp = paste0(
        "^Assertion on 'mode' failed: Must be element of set ",
        "\\{'error','warn','silent'\\}, but is '",
        mode,
        "'\\.$"
      )
    )
  }
})
