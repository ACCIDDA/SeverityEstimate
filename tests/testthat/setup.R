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
