LINE_LIST <- data.frame(
  patient = 1L:3L,
  week = c(1L, 1L, 2L),
  age = c("Youth", "Adult", "Senior"),
  detection = c("Active", "Passive", "Active"),
  outcome = c("Asymptomatic", "Death", "Symptomatic")
)

POPULATION <- data.frame(
  age = c("Youth", "Adult", "Senior"),
  amount = rep(987L, 3L)
)

MODEL <- SeverityEstimateModel(LINE_LIST, POPULATION)

# Shared fixtures for tests that exercise the precompiled `fit()` path.
#
# These live in `setup.R` so they are loaded once for the whole test suite
# instead of being duplicated in individual files. The data are intentionally
# small but still cover the active/passive surveillance and
# asymptomatic/symptomatic/death outcome branches that the Stan data assembly
# relies on.
#
# `degrees_of_freedom = 0L` is the unsmoothed categorical baseline. Tests that
# exercise smoothing should pass explicit `levels` plus `degrees_of_freedom > 0L`
# so the strata ordering is fully deterministic.
MAKE_FIT_TEST_MODEL <- function(
  strata_col = "age",
  degrees_of_freedom = 0L,
  levels = NULL
) {
  linelist <- data.frame(
    patient_id = letters,
    week = rep_len(1L:3L, 26L),
    age = rep_len(c("Female", "Male"), 26L),
    testing_type = rep_len(c("A", "A", "A", "P", "P"), 26L),
    patient_status = rep_len(c("A", "D", "S", "S"), 26L)
  )
  population <- data.frame(
    age = c("Female", "Male"),
    value = c(4000L, 3975L)
  )
  model <- SeverityEstimateModel(linelist, population) |>
    set_timesteps("week") |>
    set_detection(
      "testing_type",
      map = c("A" = "active", "P" = "passive")
    ) |>
    set_outcome(
      "patient_status",
      map = c("A" = "asymptomatic", "S" = "symptomatic", "D" = "severe")
    )
  if (!is.null(strata_col)) {
    model <- model |>
      set_strata(
        strata_col,
        levels = levels,
        degrees_of_freedom = degrees_of_freedom
      )
  }
  model
}

# Shared sampler settings for fast test-time Stan runs.
FIT_TEST_STAN_ARGS <- list(
  chains = 1L,
  iter = 100L,
  seed = 1L,
  cores = 1L,
  open_progress = FALSE,
  refresh = 0L
)
