test_that("Single-dimension design matrix expands one basis row per cell", {
  spec <- list(list(
    name = "age",
    levels = c("Youth", "Adult", "Senior"),
    degrees_of_freedom = 0L
  ))
  strata_df <- data.frame(
    age = c("Youth", "Adult", "Senior"),
    stringsAsFactors = FALSE
  )

  result <- build_strata_design_matrix(spec, strata_df)

  expect_equal(result$n_strata_basis_cols, 2L)
  expect_equal(dim(result$X_strata), c(3L, 2L))
  expect_equal(unname(colMeans(result$X_strata)), c(0, 0))
})

test_that("Mixed strata bases concatenate additively across dimensions", {
  spec <- list(
    list(
      name = "age",
      levels = c("Youth", "Adult", "Senior"),
      degrees_of_freedom = 1L
    ),
    list(
      name = "region",
      levels = c("North", "South"),
      degrees_of_freedom = 0L
    )
  )
  strata_df <- expand.grid(
    age = c("Youth", "Adult", "Senior"),
    region = c("North", "South"),
    stringsAsFactors = FALSE
  )

  result <- build_strata_design_matrix(spec, strata_df)

  expect_equal(result$n_strata_basis_cols, 2L)
  expect_equal(dim(result$X_strata), c(6L, 2L))
  expect_length(unique(result$X_strata[strata_df$region == "North", 1L]), 3L)
  expect_length(unique(result$X_strata[strata_df$age == "Youth", 2L]), 2L)
})

test_that("Complex mixed strata design matrix matches concatenated basis blocks", {
  age_levels <- c("0-4", "5-17", "18-49", "50-64", "65+")
  susceptibility_levels <- c("VeryLow", "Low", "Moderate", "High")
  vaccination_levels <- c("Vaccinated", "Unvaccinated")

  spec <- list(
    list(
      name = "age",
      levels = age_levels,
      degrees_of_freedom = 2L
    ),
    list(
      name = "susceptibility",
      levels = susceptibility_levels,
      degrees_of_freedom = 2L
    ),
    list(
      name = "vaccination",
      levels = vaccination_levels,
      degrees_of_freedom = 0L
    )
  )
  strata_df <- expand.grid(
    age = age_levels,
    susceptibility = susceptibility_levels,
    vaccination = vaccination_levels,
    stringsAsFactors = FALSE
  )

  result <- build_strata_design_matrix(spec, strata_df)

  expected_age <- build_strata_level_basis(age_levels, 2L)[
    match(strata_df$age, age_levels),
    ,
    drop = FALSE
  ]
  expected_susceptibility <- build_strata_level_basis(
    susceptibility_levels,
    2L
  )[
    match(strata_df$susceptibility, susceptibility_levels),
    ,
    drop = FALSE
  ]
  expected_vaccination <- build_strata_level_basis(vaccination_levels, 0L)[
    match(strata_df$vaccination, vaccination_levels),
    ,
    drop = FALSE
  ]
  expected <- cbind(
    expected_age,
    expected_susceptibility,
    expected_vaccination
  )
  colnames(expected) <- c(
    "age_1",
    "age_2",
    "susceptibility_1",
    "susceptibility_2",
    "vaccination_1"
  )

  expect_equal(result$n_strata_basis_cols, 5L)
  expect_equal(dim(result$X_strata), c(40L, 5L))
  expect_equal(result$X_strata, expected)
})

test_that("Design matrix rows stay aligned to a larger shuffled strata data frame", {
  age_levels <- c("0-4", "5-17", "18-49", "50-64", "65+")
  susceptibility_levels <- c("VeryLow", "Low", "Moderate", "High")
  vaccination_levels <- c("Vaccinated", "Unvaccinated")

  spec <- list(
    list(
      name = "age",
      levels = age_levels,
      degrees_of_freedom = 2L
    ),
    list(
      name = "susceptibility",
      levels = susceptibility_levels,
      degrees_of_freedom = 2L
    ),
    list(
      name = "vaccination",
      levels = vaccination_levels,
      degrees_of_freedom = 0L
    )
  )
  base_strata_df <- expand.grid(
    age = age_levels,
    susceptibility = susceptibility_levels,
    vaccination = vaccination_levels,
    stringsAsFactors = FALSE
  )
  strata_df <- base_strata_df[
    sample.int(nrow(base_strata_df), size = 250L, replace = TRUE),
    ,
    drop = FALSE
  ]

  result <- build_strata_design_matrix(spec, strata_df)

  expected_age <- build_strata_level_basis(age_levels, 2L)[
    match(strata_df$age, age_levels),
    ,
    drop = FALSE
  ]
  expected_susceptibility <- build_strata_level_basis(
    susceptibility_levels,
    2L
  )[
    match(strata_df$susceptibility, susceptibility_levels),
    ,
    drop = FALSE
  ]
  expected_vaccination <- build_strata_level_basis(vaccination_levels, 0L)[
    match(strata_df$vaccination, vaccination_levels),
    ,
    drop = FALSE
  ]
  expected <- cbind(
    expected_age,
    expected_susceptibility,
    expected_vaccination
  )
  colnames(expected) <- c(
    "age_1",
    "age_2",
    "susceptibility_1",
    "susceptibility_2",
    "vaccination_1"
  )

  expect_equal(result$n_strata_basis_cols, 5L)
  expect_equal(dim(result$X_strata), c(250L, 5L))
  expect_equal(result$X_strata, expected)
})

test_that("No-strata design matrix returns zero columns", {
  strata_df <- data.frame(.strata = 1L)

  result <- build_strata_design_matrix(list(), strata_df)

  expect_equal(result$n_strata_basis_cols, 0L)
  expect_equal(dim(result$X_strata), c(1L, 0L))
})
