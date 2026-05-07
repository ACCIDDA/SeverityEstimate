test_that("Categorical level basis uses one contrast per free level", {
  result <- build_strata_level_basis(
    c("Youth", "Adult", "Senior"),
    degrees_of_freedom = 0L
  )

  expect_equal(dim(result), c(3L, 2L))
  expect_equal(unname(colMeans(result)), c(0, 0))
  expect_identical(rownames(result), c("Youth", "Adult", "Senior"))
})

test_that("Smoothed level basis uses the requested polynomial rank", {
  result <- build_strata_level_basis(
    c("Youth", "Adult", "Senior", "Elderly"),
    degrees_of_freedom = 2L
  )

  expect_equal(dim(result), c(4L, 2L))
  expect_equal(unname(colMeans(result)), c(0, 0), tolerance = 1e-8)
  expect_identical(
    rownames(result),
    c("Youth", "Adult", "Senior", "Elderly")
  )
})

test_that("Single-level categorical basis has zero columns", {
  result <- build_strata_level_basis(
    "Overall",
    degrees_of_freedom = 0L
  )

  expect_equal(dim(result), c(1L, 0L))
  expect_identical(rownames(result), "Overall")
})

test_that("Zero-length levels trigger the internal stop", {
  expect_error(
    build_strata_level_basis(character(0L), degrees_of_freedom = 0L),
    regexp = "Internal error: `levels` must have positive length.",
    fixed = TRUE
  )
})
