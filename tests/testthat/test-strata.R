test_that("`strata()` getter returns the strata slot", {
  expect_identical(strata(MODEL), list())
  model <- MODEL |> set_strata("age")
  expect_identical(
    strata(model),
    list(list(
      name = "age",
      levels = c("Adult", "Senior", "Youth"),
      ordered = FALSE
    ))
  )
})

test_that("`set_strata()` with explicit levels modifies the strata slot", {
  expect_identical(strata(MODEL), list())
  model <- MODEL |>
    set_strata("age", levels = c("Youth", "Adult", "Senior"), ordered = TRUE)
  expect_identical(
    strata(model),
    list(list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      ordered = TRUE
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
      ordered = FALSE
    ))
  )
  expect_warning(
    strata(model) <- list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      ordered = TRUE
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
      ordered = TRUE
    ))
  )
})
