test_that("Using the `time` function modifies the 'time' slot", {
  expect_identical(MODEL@time, list())
  model <- MODEL |> time("week")
  expect_identical(
    model@time,
    list(
      name = "week",
      levels = c(1L, 2L)
    )
  )
})

test_that(
  paste0(
    "Using the `time` function with ",
    "explicit levels modifies the 'time' slot"
  ),
  {
    expect_identical(MODEL@time, list())
    model <- MODEL |> time("week", levels = 1L:4L)
    expect_identical(
      model@time,
      list(
        name = "week",
        levels = 1L:4L
      )
    )
  }
)

test_that("Overriding time raises a warning", {
  expect_identical(MODEL@time, list())
  expect_silent(model <- MODEL |> time("week"))
  expect_identical(
    model@time,
    list(
      name = "week",
      levels = c(1L, 2L)
    )
  )
  expect_warning(
    model <- model |> time("week", levels = 1L:4L),
    regexp = paste0(
      "The given 'model' has time already set to 'week'. ",
      "The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    model@time,
    list(
      name = "week",
      levels = 1L:4L
    )
  )
})
