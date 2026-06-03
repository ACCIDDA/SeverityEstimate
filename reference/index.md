# Package index

## Synthetic Data

Helpers for generating and transforming example data used to explore the
model.

- [`calculate_observed_ifr_sir()`](https://accidda.github.io/SeverityEstimate/reference/calculate_observed_ifr_sir.md)
  : Calculate Active/Passive IFR/SIR From True IFR/SIR And Detection
  Rates
- [`create_sample_linelist()`](https://accidda.github.io/SeverityEstimate/reference/create_sample_linelist.md)
  : Create A Sample Line List

## Model Specification

Constructors and accessors for defining a severity estimate model.

- [`SeverityEstimateModel()`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  [`summary(`*`<SeverityEstimateModel>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  [`print(`*`<SeverityEstimateModel>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  [`show(`*`<SeverityEstimateModel>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  [`print(`*`<SummaryEstimateModel>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
  : Severity Estimate Model
- [`default_model()`](https://accidda.github.io/SeverityEstimate/reference/default_model.md)
  : Construct A Default Severity Model
- [`detection()`](https://accidda.github.io/SeverityEstimate/reference/detection.md)
  [`` `detection<-`() ``](https://accidda.github.io/SeverityEstimate/reference/detection.md)
  [`has_detection()`](https://accidda.github.io/SeverityEstimate/reference/detection.md)
  [`require_detection()`](https://accidda.github.io/SeverityEstimate/reference/detection.md)
  [`set_detection()`](https://accidda.github.io/SeverityEstimate/reference/detection.md)
  : Get Or Set Model Detection Mapping
- [`outcome()`](https://accidda.github.io/SeverityEstimate/reference/outcome.md)
  [`` `outcome<-`() ``](https://accidda.github.io/SeverityEstimate/reference/outcome.md)
  [`has_outcome()`](https://accidda.github.io/SeverityEstimate/reference/outcome.md)
  [`require_outcome()`](https://accidda.github.io/SeverityEstimate/reference/outcome.md)
  [`set_outcome()`](https://accidda.github.io/SeverityEstimate/reference/outcome.md)
  : Get Or Set Model Outcome Mapping
- [`strata()`](https://accidda.github.io/SeverityEstimate/reference/strata.md)
  [`` `strata<-`() ``](https://accidda.github.io/SeverityEstimate/reference/strata.md)
  [`set_strata()`](https://accidda.github.io/SeverityEstimate/reference/strata.md)
  : Get Or Set Model Stratifications
- [`timesteps()`](https://accidda.github.io/SeverityEstimate/reference/timesteps.md)
  [`` `timesteps<-`() ``](https://accidda.github.io/SeverityEstimate/reference/timesteps.md)
  [`has_timesteps()`](https://accidda.github.io/SeverityEstimate/reference/timesteps.md)
  [`require_timesteps()`](https://accidda.github.io/SeverityEstimate/reference/timesteps.md)
  [`set_timesteps()`](https://accidda.github.io/SeverityEstimate/reference/timesteps.md)
  : Get Or Set Model Timesteps
- [`active_prior()`](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`` `active_prior<-`() ``](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`set_active_prior()`](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`passive_asymptomatic_prior()`](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`` `passive_asymptomatic_prior<-`() ``](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`set_passive_asymptomatic_prior()`](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`passive_symptomatic_prior()`](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`` `passive_symptomatic_prior<-`() ``](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  [`set_passive_symptomatic_prior()`](https://accidda.github.io/SeverityEstimate/reference/active_prior.md)
  : Get Or Set Model Prior Parameterizations

## Results

Fit a model and summarize posterior estimates.

- [`print(`*`<SeverityEstimateFit>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
  [`summary(`*`<SeverityEstimateFit>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
  [`print(`*`<SummaryEstimateFit>`*`)`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateFit.md)
  : Severity Estimate Fit Class
- [`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
  : Fit a severity estimate model instance.
- [`calculate_fatality_ratio()`](https://accidda.github.io/SeverityEstimate/reference/calculate_fatality_ratio.md)
  : Calculate Fatality Ratio Statistics
- [`calculate_hazard()`](https://accidda.github.io/SeverityEstimate/reference/calculate_hazard.md)
  : Calculate Hazard Statistics
- [`calculate_parameter_estimates()`](https://accidda.github.io/SeverityEstimate/reference/calculate_parameter_estimates.md)
  : Calculate Key Parameter Estimates
