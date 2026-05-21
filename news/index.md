# Changelog

## SeverityEstimate 0.1.0 (in development)

- Added `SeverityEstimateModel` S4 class to represent a model and
  contain the metadata for building a model. This forms the foundation
  for the tidymodels-esque API, like so:

``` r

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

- Added `default_model` to quickly construct a model from well formated
  population and line list `data.frame`s as a quick way to get started
  with the package.
  [\#114](https://github.com/ACCIDDA/SeverityEstimate/issues/114).
- Implemented
  [`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
  for `SeverityEstimateModel`, completing the S4 pipeline API. The model
  now supports an arbitrary number of ordered/unordered strata
  dimensions via additive fixed effects on the logit scale. Continuing
  from the example above:

``` r

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

- Added a `getting-started` vignette demonstrating the default model,
  from synthetic data generation through model fitting and result
  extraction as well as a `model-explainer` vignette to do a deep dive
  into the model.
  [\#78](https://github.com/ACCIDDA/SeverityEstimate/issues/78),
  [\#107](https://github.com/ACCIDDA/SeverityEstimate/issues/107).
- Setup `pkgdown` site hosted on GitHub pages at
  [accidda.github.io/SeverityEstimate/](https://accidda.github.io/SeverityEstimate/).
  [\#28](https://github.com/ACCIDDA/SeverityEstimate/issues/28).
- Enhanced documentation elements, such as the `README.md`,
  `CONTRIBUTING.md` to add more helpful content to the package’s
  documentation site.
  [\#3](https://github.com/ACCIDDA/SeverityEstimate/issues/3),
  [\#53](https://github.com/ACCIDDA/SeverityEstimate/issues/53).
- Restructured package utilizing
  [`rstantools`](https://mc-stan.org/rstantools/index.html).
  [\#73](https://github.com/ACCIDDA/SeverityEstimate/issues/73).
- Added support for “mean”/“concentration” parameterization of beta
  prior distributions.
  [\#71](https://github.com/ACCIDDA/SeverityEstimate/issues/71).
- Added `summary` generic for `SeverityEstimateFit` objects.
  [\#30](https://github.com/ACCIDDA/SeverityEstimate/issues/30).
- Added an integration-style notebook under `inst/notebooks/` for
  synthetic parameter-recovery checks, along with local render commands
  for heavier non-vignette R Markdown analyses.
  [\#116](https://github.com/ACCIDDA/SeverityEstimate/issues/116).
- Added `calculate_hazard` to get the underlying force of infection as a
  `data.frame` for diagnostics/additional analysis.
  [\#9](https://github.com/ACCIDDA/SeverityEstimate/issues/9).

## SeverityEstimate 0.0.1

- Initial version of the package with the main entry point being
  [`SeverityEstimate::estimate_severity`](https://accidda.github.io/SeverityEstimate/reference/estimate_severity.md)
  that implements the basics of the model from
  <https://doi.org/10.1093/aje/kwv452>.
