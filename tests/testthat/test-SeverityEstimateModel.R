LINE_LIST <- data.frame(
  id = 1L:3L,
  week = c(1L, 1L, 2L),
  sex = c("M", "F", "M"),
  outcome = c("Asymptomatic", "Symptomatic", "Death"),
  detection = c("Active", "Active", "Passive")
)

POPULATION <- data.frame(
  sex = c("Male", "Female"),
  amount = c(123L, 456L)
)

test_that("Line list and population must be data.frame like", {
  for (x in list(NULL, 123L, pi, "abc", letters)) {
    expect_error(
      SeverityEstimateModel(x, POPULATION),
      regexp = "`line_list` is not 'data.frame' like.",
      fixed = TRUE
    )
    expect_error(
      SeverityEstimateModel(LINE_LIST, x),
      regexp = "`population` is not 'data.frame' like.",
      fixed = TRUE
    )
  }
})

test_that("Create a valid SeverityEstimateModel instance", {
  model <- SeverityEstimateModel(LINE_LIST, POPULATION)
  expect_s4_class(model, "SeverityEstimateModel")
  expect_in(c("line_list", "population"), slotNames(model))
  expect_s3_class(model@line_list, "data.frame")
  expect_s3_class(model@population, "data.frame")
})
