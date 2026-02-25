# SeverityEstimate 0.1.0 (in development)

- Added `SeverityEstimateModel` S4 class to represent a model and contain the metadata for building a model. This forms the foundation for the tidymodels-esque API, like so:
```R
library(SeverityEstimate)
line_list <- data.frame(
  id = 1L:3L,
  week = c(1L, 1L, 2L),
  sex = c("M", "F", "M"),
  outcome = c("Asymptomatic", "Symptomatic", "Death"),
  detection = c("Active", "Active", "Passive")
)
population <- data.frame(
  sex = c("M", "F"),
  amount = c(123L, 456L)
)
model <- SeverityEstimateModel(line_list, population) |>
  set_active_prior(alpha = 1.0, beta = 1.0) |>
  set_passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
  set_passive_symptomatic_prior(alpha = 3.0, beta = 1.0) |>
  set_strata("sex", degrees_of_freedom = 1L) |>
  set_timesteps("week") |>
  set_detection(
    "detection",
    map = c("Active" = "active", "Passive" = "passive")
  ) |>
  set_outcome(
    "outcome",
    map = c(
      "Asymptomatic" = "asymptomatic",
      "Symptomatic" = "symptomatic",
      "Death" = "severe"
    )
  )
model
# An object of class "SeverityEstimateModel"
# Slot "line_list":
#   id week sex      outcome detection
# 1  1    1   M Asymptomatic    Active
# 2  2    1   F  Symptomatic    Active
# 3  3    2   M        Death   Passive

# Slot "population":
#   sex amount
# 1   M    123
# 2   F    456

# Slot "strata":
# [[1]]
# [[1]]$name
# [1] "sex"

# [[1]]$levels
# [1] "F" "M"

# [[1]]$ordered
# [1] FALSE

# [[1]]$degrees_of_freedom
# [1] 1



# Slot "timesteps":
# $name
# [1] "week"

# $levels
# [1] 1 2


# Slot "detection":
# $name
# [1] "detection"

# $map
#    Active   Passive
#  "active" "passive"


# Slot "outcome":
# $name
# [1] "outcome"

# $map
#   Asymptomatic    Symptomatic          Death
# "asymptomatic"  "symptomatic"       "severe"


# Slot "active_prior":
# alpha  beta
#     1     1

# Slot "passive_asymptomatic_prior":
# alpha  beta
#     1     3

# Slot "passive_symptomatic_prior":
# alpha  beta
#     3     1
```
- Implemented `fit()` for `SeverityEstimateModel`, completing the S4 pipeline API. The model now supports an arbitrary number of unordered strata dimensions via additive fixed effects on the logit scale, rendered at compile time via a Jinja2 Stan template. Continuing from the example above:
```R
fit_result <- model |>
  fit(
    chains = 2L,
    iter = 500L,
    seed = 42L,
    cores = 4L
  )
fit_result
# An object of class "SeverityEstimateFit"
# Slot "model_fit":
# Inference for Stan model: estimate_severity.stan.j2.
# 2 chains, each with iter=500; warmup=250; thin=1;
# post-warmup draws per chain=250, total post-warmup draws=500.
# ...
```
- Switched from `make` to `just` for task running. This change provides a couple of improvements namely: easier cross-platform support, simplification to task specification, and ability to invoke tasks using the shell or R. [#44](https://github.com/ACCIDDA/SeverityEstimate/issues/44).
- Added a check to CI to make sure that appropriate changes are made to `NEWS.md` file. [#61](https://github.com/ACCIDDA/SeverityEstimate/issues/61).
- Updated `renv` from v1.1.7 to v1.2.0. See [#87](https://github.com/ACCIDDA/SeverityEstimate/pull/87).

# SeverityEstimate 0.0.1

- Initial version of the package with the main entry point being `SeverityEstimate::estimate_severity` that implements the basics of the model from <https://doi.org/10.1093/aje/kwv452>.
