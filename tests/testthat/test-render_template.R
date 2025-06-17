test_that("Providing '.x' or '.config' in `template_data` raises error.", {
  expect_error(
    render_template("foobar.stan", template_data = list(.x = "abc")),
    regexp = "The names '.x' and '.config' are not allowed in `template_data`.",
    fixed = TRUE
  )
  expect_error(
    render_template("foobar.stan", template_data = list(.config = "abc")),
    regexp = "The names '.x' and '.config' are not allowed in `template_data`.",
    fixed = TRUE
  )
  expect_error(
    render_template(
      "foobar.stan",
      template_data = list(.x = "abc", .config = "xyz")
    ),
    regexp = "The names '.x' and '.config' are not allowed in `template_data`.",
    fixed = TRUE
  )
})

test_that("Renders template with no templating", {
  output <- render_template("foobar.stan")
  expect_type(output, "character")
  expect_length(output, 1L)
})

test_that("Renders template with templating", {
  output <- render_template(
    "fizzbuzz.stan.j2",
    template_data = list(mean_param = "mu")
  )
  expect_type(output, "character")
  expect_length(output, 1L)
})
