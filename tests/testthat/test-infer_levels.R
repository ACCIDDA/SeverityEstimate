test_that("The given 'name' must be a valid string", {
  for (name in list(1234L, TRUE, pi)) {
    expect_error(
      infer_levels(MODEL, name, "both"),
      regexp = paste0(
        "^Assertion on 'name' failed: Must be of type 'string', not '.*'\\.$"
      ),
      perl = TRUE
    )
  }
  expect_error(
    infer_levels(MODEL, NA_character_, "both"),
    regexp = "^Assertion on 'name' failed: May not be NA\\.$",
    perl = TRUE
  )
})

test_that("The given 'name_in' must be 'line_list', 'population', or 'both'", {
  for (name_in in c("foobar", "none", "pass")) {
    expect_error(
      infer_levels(MODEL, "age", name_in),
      regexp = paste0(
        "^Assertion on 'name_in' failed: Must be element of set ",
        "\\{'line_list','population','both'\\}, but is '",
        name_in,
        "'\\.$"
      ),
      perl = TRUE
    )
  }
})

test_that("The given 'name' must be present in the relevant data.frame(s)", {
  for (lst in list(
    list(name = "patient", name_in = "population"),
    list(name = "patient", name_in = "both"),
    list(name = "nope", name_in = "line_list"),
    list(name = "nope", name_in = "population"),
    list(name = "nope", name_in = "both"),
    list(name = "amount", name_in = "line_list"),
    list(name = "amount", name_in = "both")
  )) {
    expect_error(
      infer_levels(MODEL, lst$name, lst$name_in),
      regexp = paste0(
        "^Assertion on 'name' failed: Must be element of set \\{.*\\}, ",
        "but is '",
        lst$name,
        "'\\.$"
      ),
      perl = TRUE
    )
  }
})

test_that("Explicit levels may not contain missing or duplicated values", {
  expect_error(
    infer_levels(MODEL, "age", "both", levels = c("Youth", NA_character_)),
    regexp = "^Assertion on 'levels' failed: Contains missing values",
    perl = TRUE
  )
  expect_error(
    infer_levels(MODEL, "age", "both", levels = c("Youth", "Youth", "Adult")),
    regexp = "Assertion on 'levels' failed: Values must be unique.",
    fixed = TRUE
  )
})

test_that("Missing values in the source column are rejected", {
  model_with_missing_levels <- SeverityEstimateModel(
    data.frame(
      patient = 1L:3L,
      age = c("Youth", NA_character_, "Senior"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      age = c("Youth", "Senior"),
      amount = c(10L, 12L),
      stringsAsFactors = FALSE
    )
  )
  expect_error(
    infer_levels(model_with_missing_levels, "age", "both"),
    regexp = "^Assertion on 'levels' failed: Contains missing values",
    perl = TRUE
  )
})

test_that("Exact results for select inputs", {
  for (lst in list(
    list(
      name = "patient",
      name_in = "line_list",
      levels = NULL,
      expected = 1L:3L
    ),
    list(
      name = "patient",
      name_in = "line_list",
      levels = 4L:1L,
      expected = 4L:1L
    ),
    list(
      name = "age",
      name_in = "both",
      levels = NULL,
      expected = c("Adult", "Senior", "Youth")
    ),
    list(
      name = "age",
      name_in = "both",
      levels = c("Youth", "Adult", "Senior", "Elderly"),
      expected = c("Youth", "Adult", "Senior", "Elderly")
    ),
    list(
      name = "amount",
      name_in = "population",
      levels = NULL,
      expected = 987L
    )
  )) {
    result <- infer_levels(
      MODEL,
      lst$name,
      lst$name_in,
      levels = lst$levels
    )
    expect_equal(result, lst$expected)
  }
})
