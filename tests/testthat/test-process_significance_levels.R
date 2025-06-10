test_that("Input Validation", {
  # `stopifnot` errors
  expect_error(process_significance_levels(NULL))
  expect_error(process_significance_levels(letters))
  expect_error(process_significance_levels(NA_real_))
  expect_error(process_significance_levels(c(0.1, 0.05, NA_real_)))
  expect_error(process_significance_levels(1.00))
  expect_error(process_significance_levels(1.01))
  expect_error(process_significance_levels(0.00))
  expect_error(process_significance_levels(-0.01))
  expect_error(process_significance_levels(c(0.1, 0.05, 1.02)))
  expect_error(process_significance_levels(c(0.1, 0.05, -0.02)))
  # Custom error
  expect_error(
    process_significance_levels(c(0.101, 0.102, 0.054)),
    regexp = paste0(
      "The entries in `alpha` are not unique when rounded to 2 digits, ",
      "was given: 0.1, 0.1, 0.05."
    ),
    fixed = TRUE
  )
})

test_that("Output Validation", {
  # Single values
  for (alpha in c(0.2, 0.1, 0.05, 0.01)) {
    confidence_bounds <- process_significance_levels(alpha)
    expected_confidence_bounds <- matrix(
      data = c(0.5 * alpha, 1.0 - (0.5 * alpha)),
      nrow = 2L,
      dimnames = list(
        "bound" = c("lower", "upper"),
        "significance" = formatC(
          100.0 * alpha,
          width = 2L,
          format = "d",
          flag = "0"
        )
      )
    )
    expect_identical(confidence_bounds, expected_confidence_bounds)

    confidence_bounds2 <- process_significance_levels(alpha + 0.001)
    expect_identical(confidence_bounds2, expected_confidence_bounds)

    confidence_bounds3 <- process_significance_levels(alpha - 0.001)
    expect_identical(confidence_bounds3, expected_confidence_bounds)
  }

  # Multiple values
  alpha <- c(0.2, 0.1, 0.05, 0.01)
  confidence_bounds <- process_significance_levels(alpha)
  expected_confidence_bounds <- matrix(
    data = c(0.5 * alpha, 1.0 - (0.5 * alpha)),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(
      "bound" = c("lower", "upper"),
      "significance" = formatC(
        100.0 * alpha,
        width = 2L,
        format = "d",
        flag = "0"
      )
    )
  )
  expect_identical(confidence_bounds, expected_confidence_bounds)

  confidence_bounds2 <- process_significance_levels(alpha + 0.001)
  expect_identical(confidence_bounds2, expected_confidence_bounds)

  confidence_bounds3 <- process_significance_levels(alpha - 0.001)
  expect_identical(confidence_bounds3, expected_confidence_bounds)
})
