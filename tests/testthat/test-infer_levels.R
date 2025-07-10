test_that("The given 'name' must be a valid string", {
  for (name in list(1234L, TRUE, pi)) {
    expect_error(
      infer_levels(MODEL, name, "both"),
      regexp = paste0(
        "^Assertion on \'name\' failed\\: Must be ",
        "of type \'string\', not \'.*\'\\.$"
      ),
      perl = TRUE
    )
  }
  expect_error(
    infer_levels(MODEL, NA_character_, "both"),
    regexp = "^Assertion on \'name\' failed\\: May not be NA\\.$",
    perl = TRUE
  )
})

test_that("The given 'name_in' must be 'line_list', 'population', or 'both'", {
  for (name_in in c("foobar", "none", "pass")) {
    expect_error(
      infer_levels(MODEL, "age", name_in),
      regexp = paste0(
        "^Assertion on \'name\\_in\' failed\\: Must be element of set ",
        "\\{\'line_list\'\\,\'population\'\\,\'both\'\\}\\, but is \'",
        name_in,
        "\'\\.$"
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
        "^Assertion on \'name\' failed\\: Must be ",
        "element of set \\{.*\\}\\, but is \'",
        lst$name,
        "\'\\.$"
      ),
      perl = TRUE
    )
  }
})

test_that("The given 'ordered' must be either be TRUE or FALSE", {
  for (ordered in list(NULL, 123L, "abc", NA)) {
    expect_false(is.logical(ordered) && !is.na(ordered))
    expect_error(
      infer_levels(MODEL, "age", "both", ordered = ordered),
      regexp = "^Assertion on \'ordered\' failed\\:.*",
      perl = TRUE
    )
  }
})

test_that("Exact results for select inputs", {
  for (lst in list(
    list(
      name = "patient",
      name_in = "line_list",
      levels = NULL,
      ordered = FALSE,
      expected = 1L:3L
    ),
    list(
      name = "patient",
      name_in = "line_list",
      levels = 4L:1L,
      ordered = FALSE,
      expected = 4L:1L
    ),
    list(
      name = "patient",
      name_in = "line_list",
      levels = 4L:1L,
      ordered = TRUE,
      expected = 4L:1L
    ),
    list(
      name = "age",
      name_in = "both",
      levels = NULL,
      ordered = FALSE,
      expected = c("Adult", "Senior", "Youth")
    ),
    list(
      name = "age",
      name_in = "both",
      levels = c("Youth", "Adult", "Senior", "Elderly"),
      ordered = FALSE,
      expected = c("Youth", "Adult", "Senior", "Elderly")
    ),
    list(
      name = "age",
      name_in = "both",
      levels = c("Youth", "Adult", "Senior", "Elderly"),
      ordered = TRUE,
      expected = c("Youth", "Adult", "Senior", "Elderly")
    ),
    list(
      name = "amount",
      name_in = "population",
      levels = NULL,
      ordered = FALSE,
      expected = 987L
    )
  )) {
    result <- infer_levels(
      MODEL,
      lst$name,
      lst$name_in,
      levels = lst$levels,
      ordered = lst$ordered
    )
    expect_equal(result, lst$expected)
  }
})
