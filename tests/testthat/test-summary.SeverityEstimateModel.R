test_that("summary.SeverityEstimateModel describes a configured model", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
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
    ) |>
    set_strata(
      "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    ) |>
    set_active_prior(alpha = 2.0, beta = 3.0) |>
    set_passive_asymptomatic_prior(mean = 0.2, concentration = 10.0)

  model_summary <- summary(model)

  expect_s3_class(model_summary, "SummaryEstimateModel")
  expect_identical(
    model_summary$data,
    data.frame(
      dataset = c("line_list", "population"),
      rows = c(3L, 3L),
      columns = c(5L, 2L),
      check.names = FALSE
    )
  )
  expect_identical(
    model_summary$priors,
    data.frame(
      parameter = c(
        "active",
        "passive_asymptomatic",
        "passive_symptomatic"
      ),
      alpha = c(2.0, 2.0, 1.0),
      beta = c(3.0, 8.0, 1.0),
      default = c(FALSE, FALSE, TRUE),
      check.names = FALSE
    )
  )
  expect_identical(
    model_summary$timesteps,
    data.frame(
      column = "week",
      start = "1",
      end = "2",
      timesteps = 2L,
      check.names = FALSE
    )
  )
  expect_identical(
    model_summary$detection,
    data.frame(
      column = rep("detection", 2L),
      type = c("active", "passive"),
      values = c("Active", "Passive"),
      cases = c(2L, 1L),
      check.names = FALSE
    )
  )
  expect_identical(
    model_summary$outcome,
    data.frame(
      column = rep("outcome", 3L),
      type = c("asymptomatic", "symptomatic", "severe"),
      values = c("Asymptomatic", "Symptomatic", "Death"),
      cases = c(1L, 1L, 1L),
      check.names = FALSE
    )
  )
  expect_identical(
    model_summary$strata,
    data.frame(
      column = "age",
      degrees_of_freedom = 1L,
      levels = "Youth, Adult, Senior",
      n_levels = 3L,
      check.names = FALSE
    )
  )
})

test_that("summary.SeverityEstimateModel handles unset optional specifications", {
  model_summary <- summary(SeverityEstimateModel(LINE_LIST, 1000L))

  expect_identical(
    model_summary$data,
    data.frame(
      dataset = c("line_list", "population"),
      rows = c(3L, 1L),
      columns = c(5L, 1L),
      check.names = FALSE
    )
  )
  expect_identical(model_summary$timesteps, data.frame())
  expect_identical(model_summary$detection, data.frame())
  expect_identical(model_summary$outcome, data.frame())
  expect_identical(model_summary$strata, data.frame())
  expect_true(all(model_summary$priors$default))
})

test_that("print.SummaryEstimateModel prints the model summary", {
  model_summary <- summary(
    SeverityEstimateModel(LINE_LIST, POPULATION) |>
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
      ) |>
      set_strata("age")
  )

  printed_output <- utils::capture.output(
    # jarl-ignore implicit_assignment: capture output for testing
    returned_value <- print(model_summary, digits = 3L)
  )

  expect_identical(returned_value, model_summary)
  expect_true(any(grepl("^Severity Estimate Model:$", printed_output)))
  expect_true(any(grepl("line_list", printed_output, fixed = TRUE)))
  expect_true(any(grepl(
    "active prior: beta(1.0, 1.0)",
    printed_output,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "week: 1 to 2 (2 timesteps)",
    printed_output,
    fixed = TRUE
  )))
  expect_true(any(grepl("column: detection", printed_output, fixed = TRUE)))
  expect_true(any(grepl("active: 2 cases", printed_output, fixed = TRUE)))
  expect_true(any(grepl("severe: 1 cases", printed_output, fixed = TRUE)))
  expect_true(any(grepl("age: 3 levels, df = 0", printed_output)))
})

test_that("print.SeverityEstimateModel prints the compact model summary", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
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

  printed_output <- utils::capture.output(
    # jarl-ignore implicit_assignment: capture output for testing
    returned_value <- print(model, digits = 3L)
  )

  expect_identical(returned_value, model)
  expect_true(any(grepl("^Severity Estimate Model:$", printed_output)))
  expect_true(any(grepl("line_list", printed_output, fixed = TRUE)))
  expect_true(any(grepl("column: detection", printed_output, fixed = TRUE)))
  expect_false(any(grepl("Formal class", printed_output, fixed = TRUE)))
  expect_false(any(grepl("@line_list", printed_output, fixed = TRUE)))
})

test_that("show.SeverityEstimateModel prints the compact model summary", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION) |>
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

  printed_output <- utils::capture.output(
    # jarl-ignore implicit_assignment: capture output for testing
    returned_value <- methods::show(model)
  )

  expect_identical(returned_value, model)
  expect_true(any(grepl("^Severity Estimate Model:$", printed_output)))
  expect_true(any(grepl("line_list", printed_output, fixed = TRUE)))
  expect_true(any(grepl("column: detection", printed_output, fixed = TRUE)))
  expect_false(any(grepl("Formal class", printed_output, fixed = TRUE)))
  expect_false(any(grepl("@line_list", printed_output, fixed = TRUE)))
})
