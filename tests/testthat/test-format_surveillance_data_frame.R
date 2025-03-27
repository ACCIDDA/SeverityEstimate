test_that("Input Validation", {
  expect_error(format_surveillance_data_frame(NULL))
  expect_error(format_surveillance_data_frame(letters))
  expect_error(
    format_surveillance_data_frame(list(lower = letters, upper = LETTERS))
  )

  expect_error(
    format_surveillance_data_frame(mtcars),
    "The surveillance data.frame should only contain one column.",
    fixed = TRUE
  )

  expect_error(
    format_surveillance_data_frame(data.frame(lower = letters)),
    paste0(
      "The surveillance data.frame should ",
      "only contain 3 types of surveillance."
    ),
    fixed = TRUE
  )
  expect_error(
    format_surveillance_data_frame(data.frame(lower = as.factor(letters))),
    paste0(
      "The surveillance data.frame should ",
      "only contain 3 types of surveillance."
    ),
    fixed = TRUE
  )

  expect_error(
    format_surveillance_data_frame(data.frame(foo = c("X", "Y", "X", "Y"))),
    paste0(
      "The labels found in surveillance weren't valid. Was expecting ",
      'something like c("active", "passive", "unknown"), c("a", "p", "u").'
    ),
    fixed = TRUE
  )
  expect_error(
    format_surveillance_data_frame(
      data.frame(foo = as.factor(c("X", "Y", "X", "Y")))
    ),
    paste0(
      "The labels found in surveillance weren't valid. Was expecting ",
      'something like c("active", "passive", "unknown"), c("a", "p", "u").'
    ),
    fixed = TRUE
  )
})

test_that("Output Validation", {
  # Single letter, lower case
  surveillance <- format_surveillance_data_frame(
    data.frame(abc = c("a", "p", "u", "a", "p", "u"))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = c("a", "p", "a", "p"))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(c("a", "p", "u", "a", "p", "u")))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(c("a", "p", "a", "p")))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  # Single letter, upper case
  surveillance <- format_surveillance_data_frame(
    data.frame(abc = c("A", "P", "U", "A", "P", "U"))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = c("A", "P", "A", "P"))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(c("A", "P", "U", "A", "P", "U")))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(c("A", "P", "A", "P")))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  # Single word, lower case
  surveillance <- format_surveillance_data_frame(
    data.frame(
      abc = c("active", "passive", "unknown", "active", "passive", "unknown")
    )
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(
      abc = c("active", "passive", "active", "passive")
    )
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(
      c("active", "passive", "unknown", "active", "passive", "unknown")
    ))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(
      c("active", "passive", "active", "passive")
    ))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  # Single word, title case
  surveillance <- format_surveillance_data_frame(
    data.frame(
      abc = c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(
      abc = c("Active", "Passive", "Active", "Passive")
    )
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    ))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    ))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  # Single word, upper case
  surveillance <- format_surveillance_data_frame(
    data.frame(
      abc = c("ACTIVE", "PASSIVE", "UNKNOWN", "ACTIVE", "PASSIVE", "UNKNOWN")
    )
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(
      abc = c("ACTIVE", "PASSIVE", "ACTIVE", "PASSIVE")
    )
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(
      c("ACTIVE", "PASSIVE", "UNKNOWN", "ACTIVE", "PASSIVE", "UNKNOWN")
    ))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Unknown", "Active", "Passive", "Unknown")
    )
  )
  expect_identical(surveillance, expected_surveillance)

  surveillance <- format_surveillance_data_frame(
    data.frame(abc = as.factor(
      c("ACTIVE", "PASSIVE", "ACTIVE", "PASSIVE")
    ))
  )
  expected_surveillance <- data.frame(
    abc = as.factor(
      c("Active", "Passive", "Active", "Passive")
    )
  )
  expect_identical(surveillance, expected_surveillance)
})
