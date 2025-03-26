test_that("Generate a small non-stochastic sample with age covariate", {
  # Inputs
  strata <- data.frame(
    age = c("youth", "adult", "elderly"),
    population = c(3000, 30000, 6000),
    sir = c(0.05, 0.39, 0.88),
    ifr = c(0.11, 0.17, 0.79)
  )
  times <- seq(
    from = as.Date("2024-01-01"), to = as.Date("2024-01-31"), by = "+1 day"
  )
  active_detection <- 0.122
  passive_asymptomatic_detection <- 0.018
  passive_symptomatic_detection <- 0.956
  stochastic_case_saturation <- FALSE
  seed <- 123L

  # Call the sampler
  linelist <- create_sample_linelist(
    strata,
    times,
    active_detection,
    passive_asymptomatic_detection,
    passive_symptomatic_detection,
    stochastic_case_saturation = FALSE,
    seed = seed
  )

  # Run tests
  expect_s3_class(linelist, "data.frame", exact = TRUE)
})
