test_that("Generate a small non-stochastic sample with age covariate", {
  # Inputs
  strata <- data.frame(
    age = c("youth", "adult", "elderly"),
    population = c(3000, 30000, 6000),
    sir = c(0.1, 0.4, 0.7),
    ifr = c(0.1, 0.2, 0.3)
  )
  times <- seq(
    from = as.Date("2024-01-01"), to = as.Date("2024-01-31"), by = "+1 day"
  )
  active_detection <- 0.15
  passive_asymptomatic_detection <- 0.05
  passive_symptomatic_detection <- 0.95
  seed <- 123L

  # Call the sampler
  linelist <- create_sample_linelist(
    strata,
    times,
    active_detection,
    passive_asymptomatic_detection,
    passive_symptomatic_detection,
    seed = seed
  )

  # Run tests
  expect_s3_class(linelist, "data.frame", exact = TRUE)
})
