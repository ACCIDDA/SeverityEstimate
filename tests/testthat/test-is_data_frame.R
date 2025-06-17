test_that("Stops when given an object that is not 'data.frame' like.", {
  expect_error(
    is_data_frame(NULL),
    regexp = "`NULL` is not 'data.frame' like.",
    fixed = TRUE
  )
  expect_error(
    is_data_frame(list(abc = letters, xyz = LETTERS)),
    regexp = "`list(abc = letters, xyz = LETTERS)` is not 'data.frame' like.",
    fixed = TRUE
  )
  nums <- 1:9
  expect_error(
    is_data_frame(nums),
    regexp = "`nums` is not 'data.frame' like.",
    fixed = TRUE
  )
})

test_that("Returns the object given when exactly a 'data.frame'.", {
  sample_data_frames <- Filter(
    Negate(is.null),
    lapply(ls("package:datasets"), function(x) {
      y <- get(x, envir = asNamespace("datasets"))
      if (setequal(class(y), "data.frame")) {
        return(y)
      }
      NULL
    })
  )
  for (x in sample_data_frames) {
    y <- expect_no_error(is_data_frame(x))
    expect_identical(class(y), "data.frame")
    if (require(lobstr, quietly = TRUE)) {
      expect_identical(lobstr::obj_addr(y), lobstr::obj_addr(x))
    }
  }
})

test_that("Returns downcasted object when not given exactly a 'data.frame'", {
  sample_data_frames <- Filter(
    Negate(is.null),
    lapply(ls("package:datasets"), function(x) {
      y <- get(x, envir = asNamespace("datasets"))
      if (setequal(class(y), "data.frame")) {
        return(y)
      }
      NULL
    })
  )
  for (x in sample_data_frames) {
    class(x) <- c("not.quite.data.frame", "data.frame")
    y <- expect_no_error(is_data_frame(x))
    expect_identical(class(y), "data.frame")
    if (require(lobstr, quietly = TRUE)) {
      expect_false(identical(lobstr::obj_addr(y), lobstr::obj_addr(x)))
    }
  }
})

test_that("Stops when missing required columns", {
  x <- data.frame(
    abc = letters,
    xyz = LETTERS
  )
  expect_error(
    is_data_frame(x, has_string_columns = "ghi"),
    regexp = "`x` is missing required string columns: ghi.",
    fixed = TRUE
  )
  expect_error(
    is_data_frame(x, has_string_columns = c("ghi", "jkl")),
    regexp = "`x` is missing required string columns: ghi, jkl.",
    fixed = TRUE
  )
  expect_error(
    is_data_frame(x, has_string_columns = c("ghi", "xyz", "jkl")),
    regexp = "`x` is missing required string columns: ghi, jkl.",
    fixed = TRUE
  )
})

test_that("Stops when required columns are not characters or factors", {
  x <- data.frame(
    abc = letters,
    def = factor(
      x = rep_len(c("a", "b", "c"), 26L),
      levels = c("a", "b", "c")
    ),
    ghi = 1L:26L,
    jkl = runif(26L)
  )
  expect_error(
    is_data_frame(x, has_string_columns = "ghi"),
    regexp = paste0(
      "The 'ghi' column of `x` is not a character or factor, instead is: "
    ),
    fixed = TRUE
  )
  expect_error(
    is_data_frame(x, has_string_columns = c("abc", "ghi")),
    regexp = paste0(
      "The 'ghi' column of `x` is not a character or factor, instead is: "
    ),
    fixed = TRUE
  )
  expect_error(
    is_data_frame(x, has_string_columns = c("jkl", "ghi")),
    regexp = paste0(
      "The 'jkl' column of `x` is not a character or factor, instead is: "
    ),
    fixed = TRUE
  )
})
