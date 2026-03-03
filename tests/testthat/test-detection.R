test_that("`detection()` getter returns the detection slot", {
  expect_identical(detection(MODEL), list())
  model <- MODEL |>
    set_detection(
      "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  expect_identical(
    detection(model),
    list(
      name = "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  )
})

test_that("`set_detection()` default map works for lowercase values", {
  line_list_lower <- data.frame(
    patient = 1L:3L,
    week = c(1L, 1L, 2L),
    detection = c("active", "passive", "active"),
    outcome = c("Asymptomatic", "Death", "Symptomatic")
  )
  model_lower <- SeverityEstimateModel(line_list_lower, 1000L)
  expect_identical(detection(model_lower), list())
  model <- model_lower |> set_detection("detection")
  expect_identical(
    detection(model),
    list(
      name = "detection",
      map = c("active" = "active", "passive" = "passive")
    )
  )
})

test_that("`detection<-` replacement setter updates and can override", {
  expect_identical(detection(MODEL), list())
  model <- MODEL
  detection(model) <- list(
    name = "detection",
    map = c("Active" = "active", "Passive" = "passive")
  )
  expect_identical(
    detection(model),
    list(
      name = "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  )
  expect_warning(
    detection(model) <- list(
      name = "detection",
      map = c("Active" = "passive", "Passive" = "active")
    ),
    regexp = paste0(
      "The given 'model' has detection already set to 'detection'. ",
      "The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    detection(model),
    list(
      name = "detection",
      map = c("Active" = "passive", "Passive" = "active")
    )
  )
})

test_that("Using detection with invalid map values raises an error", {
  expect_error(
    MODEL |> set_detection("detection", map = c("Active" = "invalid")),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be one of 'active', 'passive'"
    ),
    fixed = TRUE
  )
  expect_error(
    MODEL |>
      set_detection(
        "detection",
        map = c("Active" = "active", "Passive" = "other")
      ),
    regexp = paste0(
      "Assertion on 'map' failed: All values must be one of 'active', 'passive'"
    ),
    fixed = TRUE
  )
})

test_that("Using detection with map names not in column raises an error", {
  expect_error(
    MODEL |> set_detection("detection", map = c("NotPresent" = "active")),
    regexp = "Must be a subset of \\{'Active','Passive'\\}",
    fixed = FALSE
  )
  expect_error(
    MODEL |>
      set_detection(
        "detection",
        map = c("Active" = "active", "NonExistent" = "passive")
      ),
    regexp = "Must be a subset of \\{'Active','Passive'\\}",
    fixed = FALSE
  )
})

test_that("Using detection with invalid column name raises an error", {
  expect_error(
    MODEL |> set_detection("nonexistent_column"),
    regexp = "Must be element of set \\{.*\\}",
    fixed = FALSE
  )
})

test_that("`has_detection()` returns whether detection is set", {
  expect_false(has_detection(MODEL))
  model <- MODEL |>
    set_detection(
      "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  expect_true(has_detection(model))
})

test_that("`require_detection()` checks presence according to mode", {
  expect_error(
    require_detection(MODEL),
    regexp = "No detection mapping has been set. Call `set_detection\\(\\.\\.\\.\\)` first\\."
  )
  expect_warning(
    require_detection(MODEL, mode = "warn"),
    regexp = "No detection mapping has been set. Call `set_detection\\(\\.\\.\\.\\)` first\\."
  )
  expect_identical(require_detection(MODEL, mode = "silent"), MODEL)

  model <- MODEL |>
    set_detection(
      "detection",
      map = c("Active" = "active", "Passive" = "passive")
    )
  expect_identical(require_detection(model), model)
})

test_that("`require_detection()` validates mode", {
  for (mode in c("nope", "loud", "", "warning")) {
    expect_error(
      require_detection(MODEL, mode = mode),
      regexp = paste0(
        "^Assertion on 'mode' failed: Must be element of set ",
        "\\{'error','warn','silent'\\}, but is '",
        mode,
        "'\\.$"
      )
    )
  }
})
