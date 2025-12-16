test_that("Using the `detection` function modifies the 'detection' slot", {
  expect_identical(MODEL@detection, list())
  model <- MODEL |>
    detection("detection", map = c("Active" = "active", "Passive" = "passive"))
  expect_identical(
    model@detection,
    list(
      name = "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  )
})

test_that(
  paste0(
    "Using the `detection` function with ",
    "default map works for lowercase values"
  ),
  {
    line_list_lower <- data.frame(
      patient = 1L:3L,
      week = c(1L, 1L, 2L),
      detection = c("active", "passive", "active"),
      outcome = c("Asymptomatic", "Death", "Symptomatic")
    )
    model_lower <- SeverityEstimateModel(line_list_lower, 1000L)
    expect_identical(model_lower@detection, list())
    model <- model_lower |> detection("detection")
    expect_identical(
      model@detection,
      list(
        name = "detection",
        map = c("active" = "active", "passive" = "passive")
      )
    )
  }
)

test_that("Overriding detection raises a warning", {
  expect_identical(MODEL@detection, list())
  expect_silent(
    model <- MODEL |>
      detection(
        "detection",
        map = c("Active" = "active", "Passive" = "passive")
      )
  )
  expect_identical(
    model@detection,
    list(
      name = "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  )
  expect_warning(
    model <- model |>
      detection(
        "detection",
        map = c("Active" = "passive", "Passive" = "active")
      ),
    regexp = paste0(
      "The given 'model' has detection already set to 'detection'. ",
      "The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    model@detection,
    list(
      name = "detection",
      map = c("Active" = "passive", "Passive" = "active")
    )
  )
})

test_that("Using detection with invalid map values raises an error", {
  expect_error(
    MODEL |> detection("detection", map = c("Active" = "invalid")),
    regexp = paste0(
      "Assertion on 'unname(map)' failed: Must be a subset of ",
      "{'active','passive'}, but has additional elements {'invalid'}."
    ),
    fixed = TRUE
  )
  expect_error(
    MODEL |>
      detection("detection", map = c("Active" = "active", "Passive" = "other")),
    regexp = paste0(
      "Assertion on 'unname(map)' failed: Must be a subset of ",
      "{'active','passive'}, but has additional elements {'other'}."
    ),
    fixed = TRUE
  )
})

test_that("Using detection with map names not in column raises an error", {
  expect_error(
    MODEL |> detection("detection", map = c("NotPresent" = "active")),
    regexp = "Must be a subset of \\{'Active','Passive'\\}",
    fixed = FALSE
  )
  expect_error(
    MODEL |>
      detection(
        "detection",
        map = c("Active" = "active", "NonExistent" = "passive")
      ),
    regexp = "Must be a subset of \\{'Active','Passive'\\}",
    fixed = FALSE
  )
})

test_that("Using detection with invalid column name raises an error", {
  expect_error(
    MODEL |> detection("nonexistent_column"),
    regexp = "Must be element of set \\{.*\\}",
    fixed = FALSE
  )
})
