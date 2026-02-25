test_that("`as_array()` preserves values by default", {
  expect_identical(as_array(c("a", "b")), array(c("a", "b"), dim = 2L))
  expect_identical(as_array(integer(0L)), array(integer(0L), dim = 0L))
})

test_that("`as_array()` applies the converter before building the array", {
  expect_identical(
    as_array(c(TRUE, FALSE), converter = as.integer),
    array(c(1L, 0L), dim = 2L)
  )
  expect_identical(
    as_array(c("1.5", "2.5"), converter = as.numeric),
    array(c(1.5, 2.5), dim = 2L)
  )
})

test_that("`as_integer_array()` returns a one-dimensional integer array", {
  expect_identical(
    as_integer_array(c(TRUE, FALSE, TRUE)),
    array(c(1L, 0L, 1L), dim = 3L)
  )
  expect_type(as_integer_array(1L), "integer")
})

test_that("`as_numeric_array()` returns a one-dimensional numeric array", {
  expect_identical(
    as_numeric_array(c("1", "2")),
    array(c(1, 2), dim = 2L)
  )
  expect_type(as_numeric_array(1L), "double")
})
