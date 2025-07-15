test_that("time must be specified by a model before fitting", {
  MODEL <- MODEL |>
    active_prior(c("alpha" = 1.0, "beta" = 1.0)) |>
    passive_asymptomatic_prior(c("alpha" = 1.0, "beta" = 3.0)) |>
    passive_symptomatic_prior(c("alpha" = 3.0, "beta" = 1.0))
  expect_error(
    fit(MODEL),
    regexp = paste0(
      "The time specification has not been described to this model yet. ",
      "Did you call the `SeverityEstimate::time` function on this model?"
    ),
    fixed = TRUE
  )
})
