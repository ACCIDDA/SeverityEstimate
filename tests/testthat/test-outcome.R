test_that("Using the `outcome` function modifies the 'outcome' slot", {
  expect_identical(MODEL@outcome, list())
  model <- MODEL |>
    outcome(
      "outcome",
      map = c(
        "Asymptomatic" = "asymptomatic",
        "Symptomatic" = "symptomatic",
        "Death" = "severe"
      )
    )
  expect_identical(
    model@outcome,
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

test_that(
  paste0(
    "Using the `outcome` function with ",
    "default map works for lowercase values"
  ),
  {
    line_list_lower <- data.frame(
      patient = 1L:3L,
      week = c(1L, 1L, 2L),
      outcome = c("asymptomatic", "symptomatic", "severe")
    )
    model_lower <- SeverityEstimateModel(line_list_lower, 1000L)
    expect_identical(model_lower@outcome, list())
    model <- model_lower |> outcome("outcome")
    expect_identical(
      model@outcome,
      list(
        name = "outcome",
        map = c(
          "asymptomatic" = "asymptomatic",
          "symptomatic" = "symptomatic",
          "severe" = "severe"
        )
      )
    )
  }
)

test_that("Overriding outcome raises a warning", {
  expect_identical(MODEL@outcome, list())
  expect_silent(
    model <- MODEL |>
      outcome(
        "outcome",
        map = c(
          "Asymptomatic" = "asymptomatic",
          "Symptomatic" = "symptomatic",
          "Death" = "severe"
        )
      )
  )
  expect_identical(
    model@outcome,
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
    model <- model |>
      outcome(
        "outcome",
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
    model@outcome,
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
    MODEL |> outcome("outcome", map = c("Asymptomatic" = "invalid")),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be ",
      "one of 'asymptomatic', 'symptomatic', 'severe'"
    ),
    fixed = TRUE
  )
  expect_error(
    MODEL |>
      outcome(
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
    MODEL |> outcome("outcome", map = c("NotPresent" = "asymptomatic")),
    regexp = "Must be a subset of \\{'Asymptomatic','Death','Symptomatic'\\}",
    fixed = FALSE
  )
  expect_error(
    MODEL |>
      outcome(
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
    MODEL |> outcome("nonexistent_column"),
    regexp = "Must be element of set \\{.*\\}",
    fixed = FALSE
  )
})
