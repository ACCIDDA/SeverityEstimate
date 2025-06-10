test_that("Given NULL for `values`", {
  expect_null(process_reference(NULL))
})

test_that("Given data.frame for `values`", {
  # Setup
  df <- data.frame(
    "abc" = letters,
    "def" = LETTERS,
    "ghi" = 1L:26L
  )

  # Input validation
  expect_error(
    process_reference(df, c("ghi", "jkl")),
    regexp = paste0(
      "The values given is a data.frame but is missing expected columns: jkl."
    ),
    fixed = TRUE
  )

  # Output validation
  expect_identical(process_reference(df, c("ghi")), df[, "ghi", drop = FALSE])
  expect_identical(
    process_reference(df, c("abc", "def")),
    df[, c("abc", "def")]
  )
})

test_that("Given non-NULL or non-data.frame for `values`", {
  # Input validation
  expect_error(
    process_reference(LETTERS, character()),
    regexp = paste0(
      "If a non-data.frame is provided for values only one ",
      "column can be given, but instead was given 0 columns."
    ),
    fixed = TRUE
  )
  expect_error(
    process_reference(LETTERS, c("A", "B", "C")),
    regexp = paste0(
      "If a non-data.frame is provided for values only one ",
      "column can be given, but instead was given 3 columns."
    ),
    fixed = TRUE
  )

  # Output validation
  expect_identical(process_reference(LETTERS, "abc"), data.frame(abc = LETTERS))
  expect_identical(process_reference(1L:10L, "xyz"), data.frame(xyz = 1L:10L))
})
