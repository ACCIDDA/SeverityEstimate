test_that("The inferred levels must be a subset of the explicit levels", {
  line_list <- data.frame(
    patient_id = c("UID1", "UID2", "UID3"),
    detection = c("Active", "Active", "Active"),
    outcome = c("Symptomatic", "Symptomatic", "Symptomatic")
  )
  population <- data.frame(value = 123L)
  for (lst in list(
    list("levels" = c(1L, 2L, 3L), "time" = c(1L, 2L, 4L)),
    list(
      "levels" = c("01/2024", "02/2024", "03/2024"),
      "time" = c("02/2024", "02/2024", "04/2024")
    ),
    list(
      "levels" = as.Date(c("2025-01-01", "2025-01-03", "2025-01-03")),
      "time" = as.Date(c("2025-01-03", "2025-01-04", "2025-01-05"))
    )
  )) {
    line_list$time <- lst$time
    expect_false(all(lst$time %in% lst$levels))
    expect_error(
      SeverityEstimateModel(line_list, population) |>
        time("time", levels = lst$levels),
      regexp = paste0(
        "^Assertion on \'inferred_levels\' failed\\: ",
        "Must be a subset of \\{.*\\}\\, but has ",
        "additional elements \\{.*\\}\\.$"
      ),
      perl = TRUE
    )
  }
})

test_that("Check that the time attribute of a model is set with valid call", {
  for (lst in list(
    list(
      "line_list" = data.frame(
        patient_id = c("UID1", "UID2", "UID3"),
        time = c(1L, 2L, 3L),
        detection = c("Active", "Active", "Active"),
        outcome = c("Symptomatic", "Symptomatic", "Symptomatic")
      ),
      "population" = data.frame(value = 123L),
      "name" = "time",
      "levels" = NULL
    ),
    list(
      "line_list" = data.frame(
        patient_id = c("UID1", "UID2", "UID3"),
        time = c(1L, 2L, 3L),
        detection = c("Active", "Active", "Active"),
        outcome = c("Symptomatic", "Symptomatic", "Symptomatic")
      ),
      "population" = data.frame(value = 123L),
      "name" = "time",
      "levels" = 1L:4L
    ),
    list(
      "line_list" = data.frame(
        patient_id = 1L:5L,
        date = seq(as.Date("2025-01-01"), as.Date("2025-01-05"), by = "+1 day"),
        detection = rep("Active", 5L),
        outcome = rep("Symptomatic", 5L)
      ),
      "population" = data.frame(value = 123L),
      "name" = "date",
      "levels" = NULL
    ),
    list(
      "line_list" = data.frame(
        patient_id = 1L:5L,
        date = seq(as.Date("2025-01-01"), as.Date("2025-01-05"), by = "+1 day"),
        detection = rep("Active", 5L),
        outcome = rep("Symptomatic", 5L)
      ),
      "population" = data.frame(value = 123L),
      "name" = "date",
      "levels" = seq(
        as.Date("2025-01-01"),
        as.Date("2025-01-31"),
        by = "+1 day"
      )
    )
  )) {
    model <- SeverityEstimateModel(lst$line_list, lst$population)
    expect_identical(model@time, list())
    model <- time(model, lst$name, levels = lst$levels)
    expect_length(model@time, 2L)
    expect_identical(names(model@time), c("name", "levels"))
    expect_identical(model@time$name, lst$name)
    if (!is.null(lst$levels)) {
      expect_identical(model@time$levels, lst$levels)
    } else {
      expect_set_equal(model@time$levels, lst$line_list[, lst$name])
    }
    expect_true(all(lst$line_list[, lst$name] %in% model@time$levels))
  }
})
