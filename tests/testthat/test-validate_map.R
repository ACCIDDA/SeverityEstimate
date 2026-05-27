test_that("validate_map returns NULL on valid input", {
  result <- validate_map(
    MODEL,
    "detection",
    c("Active" = "active", "Passive" = "passive"),
    c("active", "passive")
  )
  expect_null(result)
})

test_that("validate_map validates name is a string", {
  expect_error(
    validate_map(
      MODEL,
      c("detection", "outcome"), # Not a single string
      c("Active" = "active"),
      c("active", "passive")
    ),
    regexp = "Assertion on 'name' failed"
  )
  expect_error(
    validate_map(
      MODEL,
      123, # Not a string
      c("Active" = "active"),
      c("active", "passive")
    ),
    regexp = "Assertion on 'name' failed"
  )
})

test_that("validate_map checks name exists in line_list", {
  expect_error(
    validate_map(
      MODEL,
      "nonexistent_column",
      c("Active" = "active"),
      c("active", "passive")
    ),
    regexp = "Must be element of set"
  )
})

test_that("validate_map validates map is a character vector", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c(1, 2, 3), # Numeric, not character
      c("active", "passive")
    ),
    regexp = "Assertion on 'map' failed"
  )
  expect_error(
    validate_map(
      MODEL,
      "detection",
      list("Active" = "active"), # List, not vector
      c("active", "passive")
    ),
    regexp = "Assertion on 'map' failed"
  )
})

test_that("validate_map requires map to have names", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("active", "passive"), # Unnamed
      c("active", "passive")
    ),
    regexp = "Assertion on 'names\\(map\\)' failed"
  )
})

test_that("validate_map requires unique names in map", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "active", "Active" = "passive"), # Duplicate names
      c("active", "passive")
    ),
    regexp = "Assertion on 'names\\(map\\)' failed"
  )
})

test_that("validate_map requires non-empty map", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      character(0), # Empty
      c("active", "passive")
    ),
    regexp = "Assertion on 'map' failed"
  )
})

test_that("validate_map validates all map values are in valid_types", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "invalid"),
      c("active", "passive")
    ),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be one of 'active', 'passive'"
    ),
    fixed = TRUE
  )
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "active", "Passive" = "other"),
      c("active", "passive")
    ),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be one of 'active', 'passive'"
    ),
    fixed = TRUE
  )
})

test_that("validate_map validates all observed column values are mapped", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("NotPresent" = "active"),
      c("active", "passive")
    ),
    regexp = paste0(
      "The `detection` map must cover all observed values. ",
      "Missing: Active, Passive."
    ),
    fixed = TRUE
  )
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "active", "NonExistent" = "passive"),
      c("active", "passive")
    ),
    regexp = paste0(
      "The `detection` map must cover all observed values. ",
      "Missing: Passive."
    ),
    fixed = TRUE
  )
})

test_that("validate_map works with different valid_types", {
  # Test with outcome valid types
  expect_null(
    validate_map(
      MODEL,
      "outcome",
      c(
        "Asymptomatic" = "asymptomatic",
        "Death" = "severe",
        "Symptomatic" = "symptomatic"
      ),
      c("asymptomatic", "symptomatic", "severe")
    )
  )

  # Test with custom valid types
  line_list <- data.frame(
    status = c("Good", "Bad", "Neutral"),
    value = 1:3
  )
  model <- SeverityEstimateModel(line_list, 1000L)

  expect_null(
    validate_map(
      model,
      "status",
      c("Good" = "positive", "Bad" = "negative", "Neutral" = "neutral"),
      c("positive", "negative", "neutral")
    )
  )
})

test_that("validate_map allows maps for unobserved values", {
  active_only_model <- SeverityEstimateModel(
    data.frame(detection = "Active"),
    1000L
  )

  expect_null(
    validate_map(
      active_only_model,
      "detection",
      c("Active" = "active", "Passive" = "passive"),
      c("active", "passive")
    )
  )
})

test_that("validate_map can require selected valid types", {
  active_only_model <- SeverityEstimateModel(
    data.frame(detection = "Active"),
    1000L
  )

  expect_null(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "active", "Passive" = "passive"),
      c("active", "passive"),
      required_types = c("active", "passive")
    )
  )

  expect_error(
    validate_map(
      active_only_model,
      "detection",
      c("Active" = "active"),
      c("active", "passive"),
      required_types = c("active", "passive")
    ),
    regexp = paste0(
      "The `detection` map must include values for: active, passive. ",
      "Missing: passive."
    ),
    fixed = TRUE
  )
})

test_that("validate_map validates valid_types parameter", {
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "active"),
      123 # Not a character vector
    ),
    regexp = "Assertion on 'valid_types' failed"
  )
  expect_error(
    validate_map(
      MODEL,
      "detection",
      c("Active" = "active"),
      character(0) # Empty
    ),
    regexp = "Assertion on 'valid_types' failed"
  )
})

test_that("validate_map error message includes all valid_types", {
  expect_error(
    validate_map(
      MODEL,
      "outcome",
      c("Asymptomatic" = "invalid"),
      c("asymptomatic", "symptomatic", "severe")
    ),
    regexp = paste0(
      "All values must be one of 'asymptomatic', 'symptomatic', 'severe'"
    ),
    fixed = TRUE
  )
})

test_that("validate_map error message lists invalid values", {
  error_msg <- tryCatch(
    validate_map(
      MODEL,
      "outcome",
      c("Asymptomatic" = "wrong1", "Symptomatic" = "wrong2"),
      c("asymptomatic", "symptomatic", "severe")
    ),
    error = function(e) e$message
  )

  expect_match(error_msg, "Invalid values:")
  expect_match(error_msg, "wrong1")
  expect_match(error_msg, "wrong2")
})
