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
  active_prior(alpha = 1.0, beta = 1.0) |>
  passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
  passive_symptomatic_prior(alpha = 3.0, beta = 1.0) |>
  strata("sex") |>
  time("week") |>
  detection("detection", map=c("Active" = "active", "Passive" = "passive"))
model
# An object of class "SeverityEstimateModel"
# Slot "line_list":
#   id week sex      outcome detection
# 1  1    1   M Asymptomatic    Active
# 2  2    1   F  Symptomatic    Active
# 3  3    2   M        Death   Passive
#
# Slot "population":
#   sex amount
# 1   M    123
# 2   F    456
#
# Slot "strata":
# [[1]]
# [[1]]$name
# [1] "sex"
#
# [[1]]$levels
# [1] "F" "M"
#
# [[1]]$ordered
# [1] FALSE
#
#
#
# Slot "time":
# $name
# [1] "week"
#
# $levels
# [1] 1 2
#
#
# Slot "detection":
# $name
# [1] "detection"
#
# $map
#    Active   Passive 
#  "active" "passive" 
#
#
# Slot "active_prior":
# alpha  beta 
#     1     1 
#
# Slot "passive_asymptomatic_prior":
# alpha  beta 
#     1     3 
#
# Slot "passive_symptomatic_prior":
# alpha  beta 
#     3     1
```
- Switched from `make` to `just` for task running. This change provides a couple of improvements namely: easier cross-platform support, simplification to task specification, and ability to invoke tasks using the shell or R. [#44](https://github.com/ACCIDDA/SeverityEstimate/issues/44).
- Added a check to CI to make sure that appropriate changes are made to `NEWS.md` file. [#61](https://github.com/ACCIDDA/SeverityEstimate/issues/61).

# SeverityEstimate 0.0.1

- Initial version of the package with the main entry point being `SeverityEstimate::estimate_severity` that implements the basics of the model from <https://doi.org/10.1093/aje/kwv452>.
