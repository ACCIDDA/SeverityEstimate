test_that(
  paste0(
    "The `parameter` argument must be one of ",
    "'active', 'passive_asymptomatic', 'passive_symptomatic'"
  ),
  {
    for (parameter in c("abc", "def", "ghi", "", "passive", "not_active")) {
      expect_error(
        prior(MODEL, parameter, c("alpha" = 1.0, "beta" = 1.0)),
        regexp = paste0(
          "^Assertion on \'parameter\' failed\\: Must be element of set ",
          "\\{\'active\'\\,\'passive\\_asymptomatic\'\\,",
          "\'passive\\_symptomatic\\'\\}\\, but is \'",
          parameter,
          "\'\\.$"
        ),
        perl = TRUE
      )
    }
  }
)

test_that("Output validation", {
  for (parameter in c(
    "active",
    "passive_asymptomatic",
    "passive_symptomatic"
  )) {
    paramter_prior <- paste0(parameter, "_prior")
    for (params in list(
      c("alpha" = 1.0, "beta" = 2.0),
      c("mean" = 0.5, "sd" = 0.1),
      c("mean" = 0.3, "var" = 0.05)
    )) {
      model <- SeverityEstimateModel(LINE_LIST, POPULATION)
      expect_length(methods::slot(model, paramter_prior), 0L)
      model <- prior(model, parameter, params)
      expect_silent(first_slot_value <- methods::slot(model, paramter_prior))
      expect_type(first_slot_value, "double")
      expect_length(first_slot_value, 2L)
      expect_setequal(names(first_slot_value), c("alpha", "beta"))
      expect_warning(
        model <- prior(model, parameter, 0.5 * params),
        regexp = paste0(
          "^The given \'model\' has an attribute called \'",
          paramter_prior,
          "\' which has already been set\\. ",
          "The previously set value will be overridden\\.$"
        ),
        perl = TRUE
      )
      second_slot_value <- methods::slot(model, paramter_prior)
      expect_type(first_slot_value, "double")
      expect_length(first_slot_value, 2L)
      expect_setequal(names(first_slot_value), c("alpha", "beta"))
      expect_false(isTRUE(all.equal(first_slot_value, second_slot_value)))
    }
  }
})
