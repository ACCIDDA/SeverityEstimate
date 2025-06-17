test_that("Input Validation", {
  expect_error(format_outcome_data_frame(NULL))
  expect_error(format_outcome_data_frame(letters))
  expect_error(
    format_outcome_data_frame(list(lower = letters, upper = LETTERS))
  )
  expect_error(
    format_outcome_data_frame(mtcars),
    regexp = "The outcome data.frame should only contain one column.",
    fixed = TRUE
  )
  expect_error(
    format_outcome_data_frame(data.frame(lower = letters)),
    regexp = paste0(
      "The outcome data.frame should ",
      "only contain 3 types of outcome."
    ),
    fixed = TRUE
  )
  expect_error(
    format_outcome_data_frame(data.frame(lower = as.factor(letters))),
    regexp = paste0(
      "The outcome data.frame should ",
      "only contain 3 types of outcome."
    ),
    fixed = TRUE
  )
  expect_error(
    format_outcome_data_frame(
      data.frame(foo = c("X", "Y", "Z", "X", "Y", "Z"))
    ),
    regexp = paste0(
      "The labels found in outcome weren't valid. Was expecting something ",
      'like c("asymptomatic", "death", "symptomatic"), c("a", "d", "s").'
    ),
    fixed = TRUE
  )
  expect_error(
    format_outcome_data_frame(
      data.frame(foo = as.factor(c("X", "Y", "Z", "X", "Y", "Z")))
    ),
    regexp = paste0(
      "The labels found in outcome weren't valid. Was expecting something ",
      'like c("asymptomatic", "death", "symptomatic"), c("a", "d", "s").'
    ),
    fixed = TRUE
  )
})

test_that("Output Validation", {
  # For reference in this test fixture
  expected_outcome <- data.frame(
    abc = as.factor(
      c(
        "Asymptomatic",
        "Death",
        "Symptomatic",
        "Asymptomatic",
        "Death",
        "Symptomatic"
      )
    )
  )

  # Single letter, lower case
  outcome <- format_outcome_data_frame(
    data.frame(abc = c("a", "d", "s", "a", "d", "s"))
  )
  expect_identical(outcome, expected_outcome)

  outcome <- format_outcome_data_frame(
    data.frame(abc = as.factor(c("a", "d", "s", "a", "d", "s")))
  )
  expect_identical(outcome, expected_outcome)

  # Single letter, upper case
  outcome <- format_outcome_data_frame(
    data.frame(abc = c("A", "D", "S", "A", "D", "S"))
  )
  expect_identical(outcome, expected_outcome)

  outcome <- format_outcome_data_frame(
    data.frame(abc = as.factor(c("A", "D", "S", "A", "D", "S")))
  )
  expect_identical(outcome, expected_outcome)

  # Single word, lower case
  outcome <- format_outcome_data_frame(
    data.frame(
      abc = c(
        "asymptomatic",
        "death",
        "symptomatic",
        "asymptomatic",
        "death",
        "symptomatic"
      )
    )
  )
  expect_identical(outcome, expected_outcome)

  outcome <- format_outcome_data_frame(
    data.frame(
      abc = as.factor(
        c(
          "asymptomatic",
          "death",
          "symptomatic",
          "asymptomatic",
          "death",
          "symptomatic"
        )
      )
    )
  )
  expect_identical(outcome, expected_outcome)

  # Single word, title case
  outcome <- format_outcome_data_frame(
    data.frame(
      abc = c(
        "Asymptomatic",
        "Death",
        "Symptomatic",
        "Asymptomatic",
        "Death",
        "Symptomatic"
      )
    )
  )
  expect_identical(outcome, expected_outcome)

  outcome <- format_outcome_data_frame(
    data.frame(
      abc = as.factor(
        c(
          "Asymptomatic",
          "Death",
          "Symptomatic",
          "Asymptomatic",
          "Death",
          "Symptomatic"
        )
      )
    )
  )
  expect_identical(outcome, expected_outcome)

  # Single word, upper case
  outcome <- format_outcome_data_frame(
    data.frame(
      abc = c(
        "ASYMPTOMATIC",
        "DEATH",
        "SYMPTOMATIC",
        "ASYMPTOMATIC",
        "DEATH",
        "SYMPTOMATIC"
      )
    )
  )
  expect_identical(outcome, expected_outcome)

  outcome <- format_outcome_data_frame(
    data.frame(
      abc = as.factor(
        c(
          "ASYMPTOMATIC",
          "DEATH",
          "SYMPTOMATIC",
          "ASYMPTOMATIC",
          "DEATH",
          "SYMPTOMATIC"
        )
      )
    )
  )
  expect_identical(outcome, expected_outcome)
})
