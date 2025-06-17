test_that("Input Validation", {
  expect_error(construct_array_from_indices(1L:26L, 1L:26L, target = letters))
  expect_error(construct_array_from_indices(1L:26L, 1L:26L, dimnames = 1L:2L))

  expect_error(
    construct_array_from_indices(),
    regexp = "At least one vector of indices must be provided for '...'.",
    fixed = TRUE
  )

  expect_error(
    construct_array_from_indices(list(1L:26L, 1L:26L), list(1L:26L, 1L:26L)),
    regexp = paste0(
      "The first element of '...' is a list ",
      "but more than one argument was given."
    ),
    fixed = TRUE
  )

  expect_error(
    construct_array_from_indices(1L:10L, 1L:25L, 1L:50L),
    regexp = paste0(
      "The indices given for '...' must all be equal length, ",
      "instead was given arguments with lengths: 10, 25, 50."
    ),
    fixed = TRUE
  )

  expect_error(
    construct_array_from_indices(
      1L:10L,
      1L:25L,
      1L:50L,
      dim = rep.int(100L, 4L)
    ),
    regexp = paste0(
      "An explicit `dim` was given, with length 4, which ",
      "does not match the number of indicies given, 3."
    ),
    fixed = TRUE
  )
})

test_that("Small Examples", {
  test_array <- construct_array_from_indices(c(1L, 2L, 3L, 4L, 2L, 6L))
  expected_array <- array(data = c(1L, 2L, 1L, 1L, 0L, 1L), dim = 6L)
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 1L, 2L, 1L),
    c(1L, 1L, 1L, 2L, 2L)
  )
  expected_array <- array(data = c(2L, 1L, 1L, 1L), dim = c(2L, 2L))
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 1L, 2L, 1L, 2L),
    c(1L, 1L, 1L, 2L, 2L, 3L)
  )
  expected_array <- array(data = c(2L, 1L, 1L, 1L, 0L, 1L), dim = c(2L, 3L))
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 2L),
    c(1L, 2L, 1L),
    c(1L, 1L, 2L)
  )
  expected_array <- array(
    data = c(1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L),
    dim = c(2L, 2L, 2L)
  )
  expect_identical(test_array, expected_array)
})

test_that("Small Examples With Dim", {
  test_array <- construct_array_from_indices(
    c(1L, 2L, 3L, 4L, 2L, 6L),
    dim = 6L
  )
  expected_array <- array(data = c(1L, 2L, 1L, 1L, 0L, 1L), dim = 6L)
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 1L, 2L, 1L),
    c(1L, 1L, 1L, 2L, 2L),
    dim = c(2L, 3L)
  )
  expected_array <- array(data = c(2L, 1L, 1L, 1L, 0L, 0L), dim = c(2L, 3L))
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 1L, 2L, 1L, 2L),
    c(1L, 1L, 1L, 2L, 2L, 3L),
    dim = c(2L, 3L)
  )
  expected_array <- array(data = c(2L, 1L, 1L, 1L, 0L, 1L), dim = c(2L, 3L))
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 2L),
    c(1L, 2L, 1L),
    c(1L, 1L, 2L),
    dim = c(4L, 3L, 2L)
  )
  expected_array <- array(
    data = c(
      1L,
      0L,
      0L,
      0L,
      0L,
      1L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      1L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L
    ),
    dim = c(4L, 3L, 2L)
  )
  expect_identical(test_array, expected_array)
})

test_that("Small Examples With Dimnames", {
  test_array <- construct_array_from_indices(
    c(1L, 2L, 3L, 4L, 2L, 6L),
    dimnames = list("foobar" = letters[1L:6L])
  )
  expected_array <- array(
    data = c(1L, 2L, 1L, 1L, 0L, 1L),
    dim = 6L,
    dimnames = list("foobar" = letters[1L:6L])
  )
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 1L, 2L, 1L),
    c(1L, 1L, 1L, 2L, 2L),
    dimnames = c("ABC", "DEF")
  )
  expected_array <- array(
    data = c(2L, 1L, 1L, 1L),
    dim = c(2L, 2L),
    dimnames = list("ABC" = c("1", "2"), "DEF" = c("1", "2"))
  )
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 1L, 2L, 1L, 2L),
    c(1L, 1L, 1L, 2L, 2L, 3L),
    dimnames = list("char" = c("a", "b"), "int" = c("1", "2", "3"))
  )
  expected_array <- array(
    data = c(2L, 1L, 1L, 1L, 0L, 1L),
    dim = c(2L, 3L),
    dimnames = list("char" = c("a", "b"), "int" = c("1", "2", "3"))
  )
  expect_identical(test_array, expected_array)

  test_array <- construct_array_from_indices(
    c(1L, 2L, 2L),
    c(1L, 2L, 1L),
    c(1L, 1L, 2L),
    dimnames = c("foo", "bar", "baz")
  )
  expected_array <- array(
    data = c(1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L),
    dim = c(2L, 2L, 2L),
    dimnames = list(
      "foo" = c("1", "2"),
      "bar" = c("1", "2"),
      "baz" = c("1", "2")
    )
  )
  expect_identical(test_array, expected_array)
})
