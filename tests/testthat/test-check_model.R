LINE_LIST <- data.frame(
  patient = 1L:3L,
  week = c(1L, 1L, 2L),
  age = c("Youth", "Adult", "Senior"),
  detection = c("Active", "Passive", "Active"),
  outcome = c("Asymptomatic", "Death", "Symptomatic")
)

POPULATION <- data.frame(
  age = c("Youth", "Adult", "Senior"),
  amount = rep(987L, 3L)
)

MODEL <- SeverityEstimateModel(LINE_LIST, POPULATION)

test_that("The given 'model' must be a 'SeverityEstimateModel' S4 instance", {
  for (model in list(NULL, 123L, "abc")) {
    expect_false(is(model, "SeverityEstimateModel"))
    expect_error(
      check_model(model),
      regexp = paste0(
        "^Assertion on \'model\' failed\\: Must inherit from class ",
        "\'SeverityEstimateModel\'\\, but has class \'.*\'\\.$"
      ),
      perl = TRUE
    )
  }
})

test_that("The given 'attribute' must be a valid string if not NULL", {
  for (attribute in list(1234L, TRUE, pi)) {
    expect_error(
      check_model(MODEL, attribute = attribute),
      regexp = paste0(
        "^Assertion on \'attribute\' failed\\: Must be of type ",
        "\'string\' \\(or \'NULL\'\\)\\, not \'.*\'\\.$"
      ),
      perl = TRUE
    )
  }
  expect_error(
    check_model(MODEL, attribute = NA_character_),
    regexp = "^Assertion on \'attribute\' failed\\: May not be NA\\.$",
    perl = TRUE
  )
})

test_that("The given 'attribute' if not NULL must be a slot of 'model'", {
  for (attribute in c("NOPE", "not a slot of model", "incorrect")) {
    expect_false(.hasSlot(MODEL, attribute))
    expect_error(
      check_model(MODEL, attribute = attribute),
      regexp = paste0(
        "^Assertion on \'attribute\' failed\\: ",
        "Must be element of set \\{.*\\}\\, but is \'",
        attribute,
        "\'\\.$"
      ),
      perl = TRUE
    )
  }
})

test_that("Warning when the given 'attribute' is already set in 'model'", {
  MODEL@time <- list(name = "week", levels = 1L:2L)
  withr::defer(MODEL@time <- list())
  expect_warning(
    check_model(MODEL, attribute = "time"),
    regexp = paste0(
      "^The given \'model\' has an attribute called \'time\' which has ",
      "already been set\\. The previously set value will be overridden\\.$"
    ),
    perl = TRUE
  )
})
