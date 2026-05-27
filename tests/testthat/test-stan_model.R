test_that("`stan_model()` errors for an unknown precompiled model", {
  expect_error(
    stan_model("does_not_exist"),
    regexp = "Unknown Stan model",
    fixed = TRUE
  )
})
