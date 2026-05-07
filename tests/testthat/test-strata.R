test_that("`strata()` getter returns the strata slot", {
  expect_identical(strata(MODEL), list())
  model <- MODEL |> set_strata("age")
  expect_identical(
    strata(model),
    list(list(
      name = "age",
      levels = c("Adult", "Senior", "Youth"),
      degrees_of_freedom = 0L
    ))
  )
})

test_that("`set_strata()` with explicit levels modifies the strata slot", {
  expect_identical(strata(MODEL), list())
  model <- MODEL |>
    set_strata(
      "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    )
  expect_identical(
    strata(model),
    list(list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    ))
  )
})

test_that("`strata<-` replacement setter updates and can override", {
  expect_identical(strata(MODEL), list())
  model <- MODEL
  strata(model) <- list(name = "age")
  expect_identical(
    strata(model),
    list(list(
      name = "age",
      levels = c("Adult", "Senior", "Youth"),
      degrees_of_freedom = 0L
    ))
  )
  expect_warning(
    strata(model) <- list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    ),
    regexp = paste0(
      "The given 'model' has a strata called 'age' which has already ",
      "been set. The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    strata(model),
    list(list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    ))
  )
})

test_that("`set_strata()` requires explicit levels for smoothed strata", {
  expect_error(
    MODEL |> set_strata("age", degrees_of_freedom = 1L),
    regexp = paste0(
      "Assertion on 'levels' failed: Explicit levels must be provided ",
      "when `degrees_of_freedom > 0L`."
    ),
    fixed = TRUE
  )
})

test_that("`set_strata()` validates smoothed strata degrees of freedom", {
  two_level_model <- SeverityEstimateModel(
    data.frame(
      patient = 1L:2L,
      age = c("Youth", "Adult"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      age = c("Youth", "Adult"),
      amount = c(10L, 12L),
      stringsAsFactors = FALSE
    )
  )
  expect_error(
    two_level_model |>
      set_strata(
        "age",
        levels = c("Youth", "Adult"),
        degrees_of_freedom = 1L
      ),
    regexp = "Smoothed strata require at least 3 levels.",
    fixed = TRUE
  )
  expect_error(
    MODEL |>
      set_strata(
        "age",
        levels = c("Youth", "Adult", "Senior"),
        degrees_of_freedom = 2L
      ),
    regexp = paste0(
      "Assertion on 'degrees_of_freedom' failed: Must be at most 1 ",
      "for 3 levels. Use `degrees_of_freedom = 0L` for an unsmoothed ",
      "categorical effect."
    ),
    fixed = TRUE
  )
})
