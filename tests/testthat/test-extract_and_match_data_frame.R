test_that("Extract And Match Data Frame Without Subset", {
  # Setup
  x <- data.frame(
    char = rep(letters, 100L),
    num = rep_len(1L:10L, 2600L),
    log = rep_len(c(TRUE, FALSE, FALSE), 2600L)
  )

  # One column
  extract_and_match <- extract_and_match_data_frame(x, "char")
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), "char")
  expect_equal(nrow(extract_and_match$df), 26L)
  expect_equal(extract_and_match$df, data.frame(char = letters))
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(all(1L <= extract_and_match$ind & extract_and_match$ind <= 26L))
  expect_equal(extract_and_match$df$char[extract_and_match$ind], x$char)

  # Two columns
  extract_and_match <- extract_and_match_data_frame(x, c("num", "char"))
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), c("num", "char"))
  expect_equal(nrow(extract_and_match$df), 130L)
  expected_df <- unique(x[, c("num", "char")])
  expected_df <- expected_df[order(expected_df$num, expected_df$char), ]
  rownames(expected_df) <- 1L:130L
  expect_equal(extract_and_match$df, expected_df)
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(all(1L <= extract_and_match$ind & extract_and_match$ind <= 130L))
  expect_equal(extract_and_match$df$num[extract_and_match$ind], x$num)
  expect_equal(extract_and_match$df$char[extract_and_match$ind], x$char)

  # Zero columns
  extract_and_match <- extract_and_match_data_frame(x, character())
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), character())
  expect_equal(nrow(extract_and_match$df), 0L)
  expect_equal(extract_and_match$df, data.frame())
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(all(extract_and_match$ind == 1L))
})

test_that("Extract And Match Data Frame With Subset", {
  # Setup
  x <- data.frame(
    char = rep(letters, 100L),
    num = rep_len(1L:10L, 2600L),
    log = rep_len(c(TRUE, FALSE, FALSE), 2600L)
  )
  subset_x <- unique(x[, c("char", "num")])

  # One column
  extract_and_match <- extract_and_match_data_frame(
    x,
    "char",
    subset_x = subset_x
  )
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), "char")
  expect_equal(nrow(extract_and_match$df), 26L)
  expect_equal(extract_and_match$df, data.frame(char = letters))
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(all(1L <= extract_and_match$ind & extract_and_match$ind <= 26L))
  expect_equal(extract_and_match$df$char[extract_and_match$ind], x$char)

  # Two columns
  extract_and_match <- extract_and_match_data_frame(
    x,
    c("num", "char"),
    subset_x = subset_x
  )
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), c("num", "char"))
  expect_equal(nrow(extract_and_match$df), 130L)
  expected_df <- unique(x[, c("num", "char")])
  expected_df <- expected_df[order(expected_df$num, expected_df$char), ]
  rownames(expected_df) <- 1L:130L
  expect_equal(extract_and_match$df, expected_df)
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(all(1L <= extract_and_match$ind & extract_and_match$ind <= 130L))
  expect_equal(extract_and_match$df$num[extract_and_match$ind], x$num)
  expect_equal(extract_and_match$df$char[extract_and_match$ind], x$char)

  # Zero columns
  extract_and_match <- extract_and_match_data_frame(
    x,
    character(),
    subset_x = subset_x
  )
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), character())
  expect_equal(nrow(extract_and_match$df), 0L)
  expect_equal(extract_and_match$df, data.frame())
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(all(extract_and_match$ind == 1L))
})

test_that("Extract And Match Data Frame With Subset Missing Entries", {
  # Setup
  x <- data.frame(
    char = rep(letters, 100L),
    num = rep_len(1L:10L, 2600L),
    log = rep_len(c(TRUE, FALSE, FALSE), 2600L)
  )
  subset_x <- head(unique(x[, c("num", "log")]), n = 18L)

  # Tests
  expect_error(
    extract_and_match_data_frame(x, c("num", "log"), subset_x = subset_x),
    regexp = "There were values found in `x` not covered by `subset_x`.",
    fixed = TRUE
  )

  extract_and_match <- extract_and_match_data_frame(
    x,
    c("num", "log"),
    subset_x = subset_x,
    stop_on_nomatch = FALSE
  )
  expect_type(extract_and_match, "list")
  expect_length(extract_and_match, 2L)
  expect_setequal(names(extract_and_match), c("df", "ind"))
  expect_s3_class(extract_and_match$df, "data.frame")
  expect_equal(names(extract_and_match$df), c("num", "log"))
  expect_equal(nrow(extract_and_match$df), 18L)
  expected_df <- subset_x[order(subset_x$num, subset_x$log), ]
  rownames(expected_df) <- 1L:18L
  expect_equal(extract_and_match$df, expected_df)
  expect_type(extract_and_match$ind, "integer")
  expect_length(extract_and_match$ind, 2600L)
  expect_true(
    all(
      (1L <= extract_and_match$ind & extract_and_match$ind <= 130L)
      | is.na(extract_and_match$ind)
    )
  )
  not_na <- !is.na(extract_and_match$ind)
  expect_equal(
    extract_and_match$df$num[extract_and_match$ind[not_na]],
    x$num[not_na]
  )
  expect_equal(
    extract_and_match$df$log[extract_and_match$ind[not_na]],
    x$log[not_na]
  )
})
