test_that("Input Validation", {
  # Test values to work with
  linelist <- data.frame(
    patient_id = letters[1L:6L],
    week = c(1L, 1L, 1L, 2L, 2L, 3L),
    sex = c("F", "F", "M", "M", "M", "F"),
    testing = c("Active", "Passive", "Active", "Active", "Active", "Active"),
    patient_status = c("A", "A", "S", "S", "D", "S")
  )
  population <- data.frame(
    sex = c("F", "M"),
    total = c(2000L, 1950L)
  )

  # Basic tests
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "date",
      "sex",
      "testing",
      "patient_status",
      "total",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "all(time_period %in% names(linelist)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "age",
      "testing",
      "patient_status",
      "total",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "all(strata %in% names(linelist)) is not TRUE",
    fixed = TRUE
  )
  expect_error( # Specifically gets `population`
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "week",
      "testing",
      "patient_status",
      "total",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "all(strata %in% names(population)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "sex",
      "test_type",
      "patient_status",
      "total",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "all(surveillance %in% names(linelist)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "sex",
      "testing",
      "individual_outcome",
      "total",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "all(outcome %in% names(linelist)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "sex",
      "testing",
      "patient_status",
      c("total_a", "total_b"),
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "length(population_value) == 1L is not TRUE",
    fixed = TRUE
  )
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "sex",
      "testing",
      "patient_status",
      "strata_size",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = "population_value %in% names(population) is not TRUE",
    fixed = TRUE
  )
})

test_that("Incidents Strata Variables Not Covered By Population", {
  linelist <- data.frame(
    patient_id = letters[1L:7L],
    week = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    sex = c("F", "F", "M", "M", "M", "F", "NA"),
    testing = c(
      "Active", "Passive", "Active", "Active", "Active", "Active", "Passive"
    ),
    status = c("A", "S", "S", "S", "D", "D", "S")
  )
  population <- data.frame(
    sex = c("F", "M"),
    total = c(2000L, 1950L)
  )
  expect_error(
    incidence_population_arrays(
      linelist,
      population,
      "week",
      "sex",
      "testing",
      "status",
      "total",
      NULL,
      NULL,
      NULL,
      NULL
    ),
    regexp = paste0(
      "There are `strata` values found in the line ",
      "list that are not found in the population."
    ),
    fixed = TRUE
  )
})

test_that("Output Validation", {
  # Basic test
  linelist <- data.frame(
    patient_id = letters[1L:6L],
    week = c(1L, 1L, 1L, 2L, 2L, 3L),
    sex = c("F", "F", "M", "M", "M", "F"),
    testing = c("Active", "Passive", "Active", "Active", "Active", "Active"),
    patient_outcome = c(
      "Asymptomatic", "Asymptomatic", "Symptomatic",
      "Death",        "Symptomatic",  "Symptomatic"
    )
  )
  population_without_time <- data.frame(
    sex = c("F", "M"),
    total = c(2000L, 1950L)
  )
  output_arrays <- incidence_population_arrays(
    linelist,
    population_without_time,
    "week",
    "sex",
    "testing",
    "patient_outcome",
    "total",
    NULL,
    NULL,
    NULL,
    NULL
  )
  expect_equal(
    names(output_arrays),
    c(
      "incidence", "population", "time_period", "strata",
      "surveillance", "outcome", "linelist_ind"
    )
  )
  expected_incidence <- array(
    data = c(
      1L, 0L, 0L,
      0L, 0L, 0L,
      1L, 0L, 0L,
      0L, 0L, 0L,
      0L, 0L, 0L,
      0L, 1L, 0L,
      0L, 0L, 0L,
      0L, 0L, 0L,
      0L, 0L, 1L,
      1L, 1L, 0L,
      0L, 0L, 0L,
      0L, 0L, 0L
    ),
    dim = c(3L, 2L, 2L, 3L),
    dimnames = list(
      "time_period" = c("1", "2", "3"),
      "strata" = c("1", "2"),
      "surveillance" = c("1", "2"),
      "outcome" = c("1", "2", "3")
    )
  )
  expect_identical(output_arrays$incidence, expected_incidence)
  expected_population <- array(
    data = c(2000L, 1950L),
    dimnames = list("strata" = c("1", "2"))
  )
  expect_identical(output_arrays$population, expected_population)
  expected_time_period <- data.frame(week = 1L:3L)
  expect_identical(output_arrays$time_period, expected_time_period)
  expected_strata <- data.frame(sex = c("F", "M"))
  expect_identical(output_arrays$strata, expected_strata)
  expected_surveillance <- data.frame(testing = c("Active", "Passive"))
  expect_identical(output_arrays$surveillance, expected_surveillance)
  expect_identical(
    output_arrays$outcome,
    data.frame(patient_outcome = c("Asymptomatic", "Death", "Symptomatic"))
  )

  # Larger test
  linelist <- data.frame(
    patient_id = letters[1L:12L],
    month = rep(c(1L, 2L), each = 6L),
    day = c(1L, 1L, 2L, 3L, 3L, 3L, 1L, 1L, 4L, 4L, 4L, 4L),
    sex = c("F", "F", "M", "M", "M", "F", "M", "M", "F", "F", "F", "M"),
    age_category = c(
      "0-17", "0-17", "0-17", "18+", "18+", "18+",
      "0-17", "0-17", "0-17", "18+", "18+", "18+"
    ),
    testing = rep_len(c("Active", "Passive"), 12L),
    status = c(rep("Recovered", 10L), rep("Death", 2L))
  )
  population <- data.frame(
    sex = c("F", "M", "F", "M"),
    age_category = c("0-17", "0-17", "18+", "18+"),
    total = c(2000L, 1950L, 8000L, 7800L)
  )
  output_arrays <- incidence_population_arrays(
    linelist,
    population,
    c("month", "day"),
    c("sex", "age_category"),
    "testing",
    "status",
    "total",
    NULL,
    NULL,
    NULL,
    NULL
  )
  expect_equal(
    names(output_arrays),
    c(
      "incidence", "population", "time_period", "strata",
      "surveillance", "outcome", "linelist_ind"
    )
  )
  expected_incidence <- array(
    data = c(
      # 1L, 0L, 0L, 0L, 1L,
      # 0L, 0L, 0L, 0L, 1L,
      # 0L, 1L, 0L, 1L, 0L,
      # 0L, 0L, 1L, 0L, 0L,
      # 1L, 0L, 0L, 0L, 0L,
      # 0L, 0L, 1L, 0L, 1L,
      # 0L, 0L, 0L, 1L, 0L,
      # 0L, 0L, 1L, 0L, 1L

      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 1L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 1L,
      1L, 0L, 0L, 0L, 1L,
      0L, 0L, 0L, 0L, 0L,
      0L, 1L, 0L, 1L, 0L,
      0L, 0L, 1L, 0L, 0L,
      1L, 0L, 0L, 0L, 0L,
      0L, 0L, 1L, 0L, 1L,
      0L, 0L, 0L, 1L, 0L,
      0L, 0L, 1L, 0L, 0L
    ),
    dim = c(5L, 4L, 2L, 2L),
    dimnames = list(
      "time_period" = c("1", "2", "3", "4", "5"),
      "strata" = c("1", "2", "3", "4"),
      "surveillance" = c("1", "2"),
      "outcome" = c("1", "2")
    )
  )
  expect_identical(output_arrays$incidence, expected_incidence)
  expected_population <- array(
    data = c(2000L, 8000L, 1950L, 7800L),
    dimnames = list("strata" = c("1", "2", "3", "4"))
  )
  expect_identical(output_arrays$population, expected_population)
  expected_time_period <- data.frame(
    month = c(1L, 1L, 1L, 2L, 2L),
    day = c(1L, 2L, 3L, 1L, 4L)
  )
  expect_identical(output_arrays$time_period, expected_time_period)
  expected_strata <- data.frame(
    sex = c("F", "F", "M", "M"),
    age_category = c("0-17", "18+", "0-17", "18+")
  )
  expected_surveillance <- data.frame(testing = c("Active", "Passive"))
  expect_identical(output_arrays$surveillance, expected_surveillance)
  expect_identical(
    output_arrays$outcome,
    data.frame(status = c("Death", "Recovered"))
  )
  expected_linelist_ind <- matrix(
    data = c(
      1L, 1L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L,
      1L, 1L, 3L, 4L, 4L, 2L, 3L, 3L, 1L, 2L, 2L, 4L,
      1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L
    ),
    nrow = 12L,
    ncol = 4L,
    dimnames = list(
      "row" = seq_len(12L),
      "dimension" = c("time_period", "strata", "surveillance", "outcome")
    )
  )
  expect_identical(output_arrays$linelist_ind, expected_linelist_ind)
})

test_that("Output Validation When Given Reference data.frames", {
  linelist <- data.frame(
    patient_id = letters[1L:6L],
    week = c(1L, 1L, 1L, 2L, 2L, 3L),
    sex = c("F", "F", "M", "M", "M", "F"),
    testing = c("Active", "Passive", "Active", "Active", "Active", "Active"),
    patient_outcome = c(
      "Asymptomatic", "Asymptomatic", "Symptomatic",
      "Death",        "Symptomatic",  "Symptomatic"
    )
  )
  population_without_time <- data.frame(
    sex = c("F", "M"),
    total = c(2000L, 1950L)
  )
  time_period_reference <- data.frame(week = 1L:4L)
  strata_reference <- data.frame(sex = c("F", "M", "NA"))
  output_arrays <- incidence_population_arrays(
    linelist,
    population_without_time,
    "week",
    "sex",
    "testing",
    "patient_outcome",
    "total",
    time_period_reference,
    strata_reference,
    NULL,
    NULL
  )

  expect_equal(
    names(output_arrays),
    c(
      "incidence", "population", "time_period", "strata",
      "surveillance", "outcome", "linelist_ind"
    )
  )
  expected_incidence <- array(
    data = c(
      1L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      1L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 1L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 1L, 0L,
      1L, 1L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L
    ),
    dim = c(4L, 3L, 2L, 3L),
    dimnames = list(
      "time_period" = c("1", "2", "3", "4"),
      "strata" = c("1", "2", "3"),
      "surveillance" = c("1", "2"),
      "outcome" = c("1", "2", "3")
    )
  )
  expect_identical(output_arrays$incidence, expected_incidence)
  expected_population <- array(
    data = c(2000L, 1950L, 0L),
    dimnames = list("strata" = c("1", "2", "3"))
  )
  expect_identical(output_arrays$population, expected_population)
  expect_identical(output_arrays$time_period, time_period_reference)
  expect_identical(output_arrays$strata, strata_reference)
  expected_surveillance <- data.frame(testing = c("Active", "Passive"))
  expect_identical(output_arrays$surveillance, expected_surveillance)
  expect_identical(
    output_arrays$outcome,
    data.frame(patient_outcome = c("Asymptomatic", "Death", "Symptomatic"))
  )
})
