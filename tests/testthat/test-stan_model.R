test_that("By-surveillance Stan model compiles", {
  output <- rstan::stanc(
    file = system.file(
      "stan",
      "severity_estimate_by_surveillance.stan",
      package = "SeverityEstimate"
    ),
    model_name = "severity_estimate_by_surveillance",
    verbose = FALSE
  )
  expect_true(output$status)
})

test_that("`stan_model()` errors for an unknown precompiled model", {
  expect_error(
    stan_model("does_not_exist"),
    regexp = "Unknown Stan model",
    fixed = TRUE
  )
})
