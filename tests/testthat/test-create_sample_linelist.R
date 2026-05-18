test_that("Generate a small non-stochastic sample with age covariate", {
  # Inputs
  strata <- data.frame(
    age = c("youth", "adult", "elderly"),
    population = c(3000, 30000, 6000),
    sir = c(0.1, 0.4, 0.7),
    ifr = c(0.1, 0.2, 0.3)
  )
  times <- seq(
    from = as.Date("2024-01-01"),
    to = as.Date("2024-01-31"),
    by = "+1 day"
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

test_that("Generate a small no-strata sample", {
  strata <- data.frame(
    population = 50000L,
    sir = 0.40,
    ifr = 0.05
  )
  times <- seq(
    from = as.Date("2024-01-01"),
    by = "+7 days",
    length.out = 6L
  )
  force_of_infection <- matrix(
    data = 0.003,
    nrow = length(times),
    ncol = 1L
  )

  linelist <- create_sample_linelist(
    strata,
    times,
    active_detection = 0.15,
    passive_asymptomatic_detection = 0.05,
    passive_symptomatic_detection = 0.90,
    force_of_infection = force_of_infection,
    seed = 123L
  )

  expect_s3_class(linelist, "data.frame", exact = TRUE)
  expect_named(linelist, c("patient", "time", "detection", "outcome"))
  expect_true(nrow(linelist) > 0L)
  expect_true(all(linelist$detection %in% c("Active", "Passive")))
  expect_true(
    all(linelist$outcome %in% c("Asymptomatic", "Symptomatic", "Death"))
  )
})

test_that("No-strata sample input must contain exactly one row", {
  strata <- data.frame(
    population = c(50000L, 25000L),
    sir = c(0.40, 0.55),
    ifr = c(0.05, 0.08)
  )
  times <- seq(
    from = as.Date("2024-01-01"),
    by = "+7 days",
    length.out = 3L
  )

  expect_error(
    create_sample_linelist(
      strata,
      times,
      active_detection = 0.15,
      passive_asymptomatic_detection = 0.05,
      passive_symptomatic_detection = 0.90,
      seed = 123L
    ),
    regexp = paste0(
      "When `strata` only contains `population`, `sir`, and `ifr`, ",
      "it must contain exactly one row."
    ),
    fixed = TRUE
  )
})
