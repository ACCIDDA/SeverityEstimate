test_that("Using the `strata` function modifies the 'strata' slot", {
  expect_identical(MODEL@strata, list())
  model <- MODEL |> strata("age")
  expect_identical(
    model@strata,
    list(list(
      name = "age",
      levels = c("Adult", "Senior", "Youth"),
      ordered = FALSE
    ))
  )
})

test_that(
  paste0(
    "Using the `strata` function with ",
    "explicit levels modifies the 'strata' slot"
  ),
  {
    expect_identical(MODEL@strata, list())
    model <- MODEL |>
      strata("age", levels = c("Youth", "Adult", "Senior"), ordered = TRUE)
    expect_identical(
      model@strata,
      list(list(
        name = "age",
        levels = c("Youth", "Adult", "Senior"),
        ordered = TRUE
      ))
    )
  }
)

test_that("Overriding a strata raises a warning", {
  expect_identical(MODEL@strata, list())
  expect_silent(model <- MODEL |> strata("age"))
  expect_identical(
    model@strata,
    list(list(
      name = "age",
      levels = c("Adult", "Senior", "Youth"),
      ordered = FALSE
    ))
  )
  expect_warning(
    model <- model |>
      strata("age", levels = c("Youth", "Adult", "Senior"), ordered = TRUE),
    regexp = paste0(
      "The given 'model' has a strata called 'age' which has already ",
      "been set. The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    model@strata,
    list(list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      ordered = TRUE
    ))
  )
})
