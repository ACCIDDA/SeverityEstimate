library(lobstr)

test_that("Stops when given an object that is not 'data.frame' like.", {
  expect_error(
    is_data_frame(NULL),
    "`NULL` is not 'data.frame' like.",
    fixed = TRUE
  )
  expect_error(
    is_data_frame(list(abc = letters, xyz = LETTERS)),
    "`list(abc = letters, xyz = LETTERS)` is not 'data.frame' like.",
    fixed = TRUE
  )
  nums <- 1:9
  expect_error(
    is_data_frame(nums),
    "`nums` is not 'data.frame' like.",
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
