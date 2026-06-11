# `SeverityEstimate`

`SeverityEstimate` is an R package for Bayesian estimation of infection severity from case line-list data. It is designed for settings where the same outbreak is observed through both active and passive surveillance, opposed to severity estimates based only on observed cases that can be biased toward symptomatic and severe infections. The package is based on Lessler et al. (2016) <doi:10.1093/aje/kwv452>.

## Why use `SeverityEstimate`?

`SeverityEstimate` is built for working with line list data where each case can be tied to whether the case was observed from passive or active surveillance. Active surveillance is more likely to identify mild or asymptomatic infections, while passive surveillance is typically enriched for symptomatic and severe presentations. `SeverityEstimate` models those surveillance processes explicitly and estimates quantities such as the infection fatality ratio (IFR), the symptomatic infection rate (SIR), and the surveillance detection probabilities.

A useful comparison point is [`cfr`](https://epiverse-trace.github.io/cfr/). `cfr` is aimed at delay-adjusted case fatality risk estimation from epidemic time-series data. `SeverityEstimate`, by contrast, uses line-list data plus explicit active/passive surveillance labels to infer population severity. If you only have cases and deaths over time, `cfr` is likely the better fit. If you have individual-level records with both surveillance source and outcome, `SeverityEstimate` is the more natural model.

## Installation

To install `SeverityEstimate` from CRAN you can run the following:

```r
install.packages("SeverityEstimate")
```

### Installing From Source

The simpliest way to install `SeverityEstimate` from source is via `pak::pkg_download()`:

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

J.L. was supported in part by the RAPIDD program of the Science & Technology Directorate, Department of Homeland Security, and the Fogarty International Center, National Institutes of Health. D.A.T.C. acknowledges funding from the US National Institute of General Medical Sciences (grant 5U54GM088491, Computational Models of Infectious Disease Threats). N.M.F. and S.C. acknowledge funding from the Medical Research Council, the National Institute of Health Research for Health Protection Research Unit programme, Labex IBEID, the European Union Seventh Framework Programme (FP7/2007–2013) under grant agreement number 278433-PREDEMICS, the NIGMS MIDAS initiative, the Bill and Melinda Gates Foundation, and the AXA Research Fund.

The members of the MERS-CoV Scenario Modeling Working Group are Homud Algarni, Khalid AlHarbi, Hannah Clapham, Caitlin Collins, Anne Cori, Christl Donnelly, Christophe Fraser, Tini Garske, M. Kate Grabowski, Harriet Mills, Sean M. Moore, Pierre Nouvellet, Steven Riley, Shaun Truelove, and Abdulhafiz Turkistani.
