test_that("reorder_by_strata follows the given strata reference rows", {
  strata <- data.frame(
    age = rep(c("youth", "adult", "senior"), each = 2L),
    health_occupation = rep(c("no", "yes"), times = 3L)
  )
  shuffled_idx <- sample.int(nrow(strata))
  x <- strata[shuffled_idx, , drop = FALSE]
  x$row_id <- shuffled_idx

  expect_identical(
    reorder_by_strata(x, strata),
    data.frame(
      age = rep(c("youth", "adult", "senior"), each = 2L),
      health_occupation = rep(c("no", "yes"), times = 3L),
      row_id = 1L:6L
    )
  )
})

test_that("reorder_by_strata is a no-op when there are no strata columns", {
  x <- data.frame(value = c(2L, 1L))
  strata <- data.frame(.strata = 1L)[, character(0L), drop = FALSE]

  expect_identical(reorder_by_strata(x, strata), x)
})

test_that("reorder_by_strata errors when strata columns are missing from x", {
  x <- data.frame(age = c("youth", "adult"))
  strata <- data.frame(
    age = c("youth", "adult"),
    health_occupation = c("no", "yes")
  )

  expect_error(
    reorder_by_strata(x, strata),
    regexp = paste0(
      "The given `x` is missing strata columns: health_occupation\\."
    )
  )
})

test_that("reorder_by_strata errors when x contains rows not covered by strata", {
  x <- data.frame(
    age = c("youth", "adult"),
    health_occupation = c("no", "maybe")
  )
  strata <- data.frame(
    age = c("youth", "adult"),
    health_occupation = c("no", "yes")
  )

  expect_error(
    reorder_by_strata(x, strata),
    regexp = paste0(
      "The given `x` contains strata rows not covered by `strata`\\."
    )
  )
})
