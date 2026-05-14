# `SeverityEstimate`

`SeverityEstimate` is an R package for Bayesian estimation of infection severity from case line-list data. It is designed for settings where the same outbreak is observed through both active and passive surveillance, opposed to severity estimates based only on observed cases that can be biased toward
symptomatic and severe infections. The package is based on [Lessler et al. (2016)](https://doi.org/10.1093/aje/kwv452).

## Why use `SeverityEstimate`?

`SeverityEstimate` is built for working with line list data where each case can be tied to whether the case was observed from passive or active surveillance. Active surveillance is more likely to identify mild or asymptomatic infections, while passive surveillance is typically enriched for symptomatic and severe presentations. `SeverityEstimate` models those surveillance processes explicitly and estimates quantities such as the infection fatality ratio (IFR), the symptomatic infection rate (SIR), and the surveillance detection probabilities.

A useful comparison point is [`cfr`](https://epiverse-trace.github.io/cfr/). `cfr` is aimed at delay-adjusted case fatality risk estimation from epidemic time-series data. `SeverityEstimate`, by contrast, uses line-list data plus explicit active/passive surveillance labels to infer population severity. If you only have cases and deaths over time, `cfr` is likely the better fit. If you have individual-level records with both surveillance source and outcome, `SeverityEstimate` is the more natural model.

## Installation

At the moment `SeverityEstimate` is distributed as source. You can download the package bundle with `pak::pkg_download()`:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_download(
  "ACCIDDA/SeverityEstimate",
  dest_dir = "pkg-src",
  platforms = "source"
)
```

Because the package depends on `rstan`, installation requires a working source package toolchain and `GNU make`. After downloading the source bundle, install it using your usual local-package workflow.

## Minimal example

```r
library(SeverityEstimate)

linelist <- data.frame(
  patient = seq_len(12),
  week = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L),
  detection = c(
    "Active", "Active", "Passive",
    "Active", "Passive", "Passive",
    "Active", "Passive", "Passive",
    "Active", "Passive", "Passive"
  ),
  outcome = c(
    "Asymptomatic", "Symptomatic", "Symptomatic",
    "Asymptomatic", "Asymptomatic", "Death",
    "Symptomatic", "Symptomatic", "Death",
    "Death", "Symptomatic", "Death"
  )
)

model <- SeverityEstimateModel(linelist, population = 50000L) |>
  set_active_prior(alpha = 1, beta = 1) |>
  set_passive_asymptomatic_prior(alpha = 1, beta = 3) |>
  set_passive_symptomatic_prior(alpha = 3, beta = 1) |>
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

estimate <- fit(
  model,
  chains = 2L,
  iter = 250L,
  seed = 123L,
  cores = 2L,
  open_progress = FALSE,
  refresh = 0L
)

calculate_fatality_ratio(
  estimate,
  mean_estimate = FALSE,
  median_estimate = TRUE
)
```

This example intentionally uses small sampler settings to stay lightweight. For real analyses, use more iterations and review the usual Stan diagnostics. For a fuller walkthrough, including stratified analyses, see `vignette("getting-started")`, or for a detailed explaination of the model see `vignette("model-explainer")`.

## Funding Acknowledgement

This project was made possible by cooperative agreement CDC-RFA-FT-23-0069 from the CDC's Center for Forecasting and Outbreak Analytics. Its contents are solely the responsibility of the authors and do not necessarily represent the official views of the Centers for Disease Control and Prevention.
