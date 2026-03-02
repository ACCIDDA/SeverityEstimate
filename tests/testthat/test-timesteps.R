test_that("`timesteps()` getter returns the timesteps slot", {
  expect_identical(timesteps(MODEL), list())
  model <- MODEL |> set_timesteps("week")
  expect_identical(
    timesteps(model),
    list(
      name = "week",
      levels = c(1L, 2L)
    )
  )
})

test_that("`has_timesteps()` returns whether timesteps are set", {
  expect_false(has_timesteps(MODEL))
  model <- MODEL |> set_timesteps("week")
  expect_true(has_timesteps(model))
})

test_that("`require_timesteps()` checks presence according to mode", {
  expect_error(
    require_timesteps(MODEL),
    regexp = "No timesteps have been set. Call `set_timesteps\\(\\.\\.\\.\\)` first\\."
  )
  expect_warning(
    require_timesteps(MODEL, mode = "warn"),
    regexp = "No timesteps have been set. Call `set_timesteps\\(\\.\\.\\.\\)` first\\."
  )
  expect_identical(require_timesteps(MODEL, mode = "silent"), MODEL)

  model <- MODEL |> set_timesteps("week")
  expect_identical(require_timesteps(model), model)
})

test_that("`require_timesteps()` validates mode", {
  for (mode in c("nope", "loud", "", "warning")) {
    expect_error(
      require_timesteps(MODEL, mode = mode),
      regexp = paste0(
        "^Assertion on 'mode' failed: Must be element of set ",
        "\\{'error','warn','silent'\\}, but is '",
        mode,
        "'\\.$"
      )
    )
  }
})

test_that("`set_timesteps()` with explicit levels modifies the timesteps slot", {
  expect_identical(timesteps(MODEL), list())
  model <- MODEL |> set_timesteps("week", levels = 1L:4L)
  expect_identical(
    timesteps(model),
    list(
      name = "week",
      levels = 1L:4L
    )
  )
})

test_that("`timesteps<-` replacement setter updates and can override", {
  expect_identical(timesteps(MODEL), list())
  model <- MODEL
  timesteps(model) <- list(name = "week")
  expect_identical(
    timesteps(model),
    list(
      name = "week",
      levels = c(1L, 2L)
    )
  )
  expect_warning(
    timesteps(model) <- list(name = "week", levels = 1L:4L),
    regexp = paste0(
      "The given 'model' has timesteps already set to 'week'. ",
      "The previously set value will be overridden."
    ),
    fixed = TRUE
  )
  expect_identical(
    timesteps(model),
    list(
      name = "week",
      levels = 1L:4L
    )
  )
})
