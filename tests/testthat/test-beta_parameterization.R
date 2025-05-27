test_that("`params` must be length 2, be finite, and have strict naming", {
  expect_error(
    beta_parameterization(NULL),
    regexp = "Must be of type 'numeric', not 'NULL'"
  )
  expect_error(
    beta_parameterization(c(TRUE, FALSE)),
    regexp = "Must be of type 'numeric', not 'logical'"
  )
  expect_error(
    beta_parameterization(c(1.0, 2.0)),
    regexp = "Must have names"
  )
  expect_error(
    beta_parameterization(c("abc" = 2.0, "abc" = 3.0)),
    regexp = "Must have unique names, but element 2 is duplicated"
  )
  expect_error(
    beta_parameterization(c("abc" = 2.0, "$" = 3.0)),
    regexp = paste0(
      "Must have names according to R's variable naming ",
      "conventions, but element 2 does not comply"
    )
  )
  expect_error(
    beta_parameterization(c("a" = 1.0, "b" = 2.0, "c" = 3.0)),
    regexp = "Must have length 2, but has length 3"
  )
  expect_error(
    beta_parameterization(c("a" = 1.0)),
    regexp = "Must have length 2, but has length 1"
  )
  expect_error(
    beta_parameterization(c("a" = 1.0, "b" = Inf)),
    regexp = "Must be finite"
  )
  expect_error(
    beta_parameterization(c("a" = 1.0, "b" = -Inf)),
    regexp = "Must be finite"
  )
  expect_error(
    beta_parameterization(c("a" = 1.0, "b" = NA_real_)),
    regexp = "Contains missing values (element 2)",
    fixed = TRUE
  )
  expect_error(
    beta_parameterization(c("a" = NA_real_, "b" = NA_real_)),
    regexp = "Contains missing values (element 1)",
    fixed = TRUE
  )
})

test_that("`params` must have a specific set of names", {
  invalid_params <- list(
    c("alpha" = 1.0, "gamma" = 2.0),
    c("mean" = 0.5, "std" = 0.1),
    c("mode" = 0.25, "var" = 0.05),
    c("mean" = 0.75, "beta" = 2.0)
  )
  for (params in invalid_params) {
    expect_error(
      beta_parameterization(params),
      regexp = paste0(
        "The given parameterization ", toString(names(params)), " is not ",
        "recognized. Must be one of 'alpha'/'beta', 'mean'/'var', 'mean'/'sd'."
      ),
      fixed = TRUE
    )
  }
})

test_that("Input validation for `params` when 'alpha'/'beta' parameterized", {
  invalid_params <- list(
    c("alpha" = 0.0, "beta" = 1.0),
    c("beta" = 0.0, "alpha" = 1.0),
    c("alpha" = 0.0, "beta" = 0.0)
  )
  for (params in invalid_params) {
    expect_setequal(names(params), c("alpha", "beta"))
    expect_error(
      beta_parameterization(params),
      regexp = "Element [1|2] is not >="
    )
  }
})

test_that("Output validation for `params` when 'alpha'/'beta' parameterized", {
  valid_params <- list(
    c("alpha" = 1.0, "beta" = 1.0),
    c("beta" = 1.0, "alpha" = 1.0),
    c("alpha" = 3.45, "beta" = 0.56),
    c("beta" = 23.4, "alpha" = 56.7)
  )
  for (params in valid_params) {
    expect_setequal(names(params), c("alpha", "beta"))
    new_params <- expect_no_error(beta_parameterization(params))
    expect_identical(new_params, params)
  }
})

test_that(
  paste0(
    "Input validation for `params` when ",
    "'mean'/'var' or 'mean'/'sd' parameterized"
  ),
  {
    # Tests for 'mean'
    invalid_mean_params <- list(
      c("mean" = 2.0, "sd" = 0.1),
      c("mean" = -0.5, "sd" = 0.1),
      c("mean" = 0.0, "sd" = 0.1),
      c("mean" = 1.0, "sd" = 0.1)
    )
    for (params in invalid_mean_params) {
      expect_true(
        setequal(names(params), c("mean", "var")) ||
          setequal(names(params), c("mean", "sd"))
      )
      expect_error(
        beta_parameterization(params),
        regexp = "Element 1 is not",
        fixed = TRUE
      )
    }
    # Tests for 'var'/'sd'
    invalid_disp_params <- list(
      c("mean" = 0.5, "sd" = -1.0),
      c("mean" = 0.5, "sd" = 0.0),
      c("mean" = 0.5, "var" = -0.5),
      c("mean" = 0.5, "var" = 0.0)
    )
    for (params in invalid_disp_params) {
      expect_true(
        setequal(names(params), c("mean", "var")) ||
          setequal(names(params), c("mean", "sd"))
      )
      expect_error(
        beta_parameterization(params),
        regexp = "Element 1 is not >=",
        fixed = TRUE
      )
    }
  }
)

test_that(
  paste0(
    "Output validation for `params` when ",
    "'mean'/'var' or 'mean'/'sd' parameterized"
  ),
  {
    valid_params <- list(
      c("mean" = 0.5, "var" = 0.1),
      c("mean" = 0.75, "var" = 0.1),
      c("mean" = 0.25, "var" = 0.1),
      c("mean" = 0.5, "sd" = 0.1),
      c("mean" = 0.75, "sd" = 0.1),
      c("mean" = 0.25, "sd" = 0.1)
    )
    for (params in valid_params) {
      expect_true(
        setequal(names(params), c("mean", "var")) ||
          setequal(names(params), c("mean", "sd"))
      )
      new_params <- expect_no_error(beta_parameterization(params))
      expect_setequal(names(new_params), c("alpha", "beta"))
      if (params["mean"] < 0.5) {
        expect_gt(new_params["beta"], new_params["alpha"])
      } else if (params["mean"] > 0.5) {
        expect_gt(new_params["alpha"], new_params["beta"])
      } else {
        expect_equal(
          new_params["alpha"],
          new_params["beta"],
          ignore_attr = TRUE
        )
      }
    }
  }
)
