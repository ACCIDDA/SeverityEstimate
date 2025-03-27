test_that("Input Validation", {
  expect_error(format_extract_data_frame(NULL, "foo", list(letters)))
  expect_error(format_extract_data_frame(letters, "foo", list(letters)))
  expect_error(
    format_extract_data_frame(
      data.frame(abc = letters, def = LETTERS),
      "foo",
      list(letters)
    )
  )
  expect_error(
    format_extract_data_frame(
      data.frame(abc = letters),
      "foo",
      list("a", c("b", "c"), c("d", "e", "f"))
    ),
    "Was given inconsistent valid level lengths: 1, 2, 3.",
    fixed = TRUE
  )
  expect_error(
    format_extract_data_frame(
      data.frame(abc = letters),
      "foo",
      list(c("a", "b", "c"), c("d", "e", "f"))
    ),
    "The foo data.frame should only contain 3 types of foo.",
    fixed = TRUE
  )
  expect_error(
    format_extract_data_frame(
      data.frame(abc = c("x", "y")),
      "foo",
      list(c("a", "b"), c("c", "d"))
    ),
    paste0(
      "The labels found in foo weren't valid. ",
      'Was expecting something like c("a", "b"), c("c", "d").'
    ),
    fixed = TRUE
  )
  expect_error(
    format_extract_data_frame(
      data.frame(abc = c("A", "B")),
      "foo",
      list(c("a", "b")),
      case_insensitive_levels = FALSE
    ),
    paste0(
      "The labels found in foo weren't valid. ",
      'Was expecting something like c("a", "b").'
    ),
    fixed = TRUE
  )
})
