test_that("Renders stan model with no templating", {
  output <- stan_model("foobar.stan", fun = rstan::stanc, verbose = FALSE)
  expect_true(output$status)
})

test_that("Renders stan model with templating", {
  output <- stan_model(
    "fizzbuzz.stan.j2",
    data = list(mean_param = "mu"),
    fun = rstan::stanc,
    verbose = FALSE
  )
  expect_true(output$status)
})
