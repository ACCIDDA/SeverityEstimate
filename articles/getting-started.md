# Getting Started

## Overview

`SeverityEstimate` fits a Bayesian model to estimate the infection
fatality ratio (IFR) and symptomatic infection rate (SIR) from case line
list data, accounting for the fact that active and passive surveillance
systems detect cases at different rates and with different symptom
profiles. The model is based on Lessler et al. (2016)
<doi:10.1093/aje/kwv452>.

The lightest workflow is to format your data with `time`, `detection`,
and `outcome` columns, plus any strata columns you want to model, then
call
[`default_model()`](https://accidda.github.io/SeverityEstimate/reference/default_model.md)
followed by
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md).
For more customized models, you can still build a
`SeverityEstimateModel` manually and configure it with the individual
`set_*()` helpers.

``` r

old <- options(mc.cores = 1L)
on.exit(options(old))
library(SeverityEstimate)
#> Loading required package: checkmate
#> Loading required package: rstan
#> Loading required package: StanHeaders
#> 
#> rstan version 2.32.7 (Stan version 2.32.2)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
#> For within-chain threading using `reduce_sum()` or `map_rect()` Stan functions,
#> change `threads_per_chain` option:
#> rstan_options(threads_per_chain = 1)
```

## Sample data

We start by generating a synthetic line list with
[`create_sample_linelist()`](https://accidda.github.io/SeverityEstimate/reference/create_sample_linelist.md).
This requires a `strata` table describing each subgroup’s true
underlying parameters - the infection fatality ratio (`ifr`), the
symptomatic infection rate (`sir`), and the population size — along with
a sequence of observation times and detection probabilities for active
and passive surveillance.

Here we define three age groups observed weekly over a 13-week period,
with a constant force of infection of 0.005 across all strata and time
points.

``` r

strata <- do.call(rbind, lapply(list(
  list(age = "youth",  ifr = 0.25, sir = 0.50, population = 10000L),
  list(age = "adult",  ifr = 0.30, sir = 0.65, population = 20000L),
  list(age = "senior", ifr = 0.35, sir = 0.80, population = 5000L)
), as.data.frame))
times <- seq(as.Date("2024-01-01"), as.Date("2024-3-31"), "+7 days")
force_of_infection <- matrix(
  data = 0.005, nrow = length(times), ncol = nrow(strata)
)
linelist <- create_sample_linelist(
  strata,
  times,
  0.15,  # active detection probability
  0.05,  # passive detection probability for asymptomatic cases
  0.95,  # passive detection probability for symptomatic cases
  force_of_infection = force_of_infection,
  seed = 123L
)
head(linelist, n = 20L)
#>    patient       time   age detection      outcome
#> 1     UID1 2024-01-01 youth    Active Asymptomatic
#> 2     UID2 2024-01-01 youth    Active Asymptomatic
#> 3     UID3 2024-01-01 youth    Active Asymptomatic
#> 4     UID4 2024-01-01 youth    Active Asymptomatic
#> 5     UID5 2024-01-01 youth    Active  Symptomatic
#> 6     UID6 2024-01-01 youth    Active  Symptomatic
#> 7     UID7 2024-01-08 youth    Active Asymptomatic
#> 8     UID8 2024-01-08 youth    Active Asymptomatic
#> 9     UID9 2024-01-08 youth    Active Asymptomatic
#> 10   UID10 2024-01-08 youth    Active Asymptomatic
#> 11   UID11 2024-01-08 youth    Active Asymptomatic
#> 12   UID12 2024-01-08 youth    Active Asymptomatic
#> 13   UID13 2024-01-08 youth    Active  Symptomatic
#> 14   UID14 2024-01-08 youth    Active  Symptomatic
#> 15   UID15 2024-01-08 youth    Active  Symptomatic
#> 16   UID16 2024-01-08 youth    Active        Death
#> 17   UID17 2024-01-08 youth    Active        Death
#> 18   UID18 2024-01-08 youth    Active        Death
#> 19   UID19 2024-01-15 youth    Active Asymptomatic
#> 20   UID20 2024-01-15 youth    Active Asymptomatic
```

The resulting line list has one row per observed case, with columns for
the patient identifier, time step, age stratum, detection type (`Active`
or `Passive`), and outcome (`Asymptomatic`, `Symptomatic`, or `Death`).

``` r

summary(linelist)
#>    patient               time                age             detection        
#>  Length:1502        Min.   :2024-01-01   Length:1502        Length:1502       
#>  Class :character   1st Qu.:2024-01-22   Class :character   Class :character  
#>  Mode  :character   Median :2024-02-12   Mode  :character   Mode  :character  
#>                     Mean   :2024-02-12                                        
#>                     3rd Qu.:2024-03-04                                        
#>                     Max.   :2024-03-25                                        
#>    outcome         
#>  Length:1502       
#>  Class :character  
#>  Mode  :character  
#>                    
#>                    
#> 
```

## Building and fitting the model

[`default_model()`](https://accidda.github.io/SeverityEstimate/reference/default_model.md)
is the shortest path from formatted data to a fitted model. It expects
the line list to contain `time`, `detection`, and `outcome`, and it
treats every other line-list column as a strata column with
`degrees_of_freedom = 0L`. Because the sample line list also contains a
patient identifier, we first drop that column before constructing the
model.

[`default_model()`](https://accidda.github.io/SeverityEstimate/reference/default_model.md)
also sets the weakly informative detection priors used throughout this
package:

- Active detection: `Beta(1, 1)`
- Passive asymptomatic detection: `Beta(1, 3)`
- Passive symptomatic detection: `Beta(3, 1)`

Once constructed,
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
samples from the package’s precompiled Stan model, passing any
additional arguments (e.g. `chains`, `iter`) through to
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).
The settings below are intentionally small to keep this vignette
reasonably quick to render. They are not sufficient to assess
convergence or support serious inference. In practice you’d want to use
more chains and iterations and review the usual Stan diagnostics.

``` r

linelist_model_data <- linelist[, c("time", "age", "detection", "outcome")]
population <- strata[, c("age", "population")]

model <- default_model(linelist_model_data, population)
model
#> Severity Estimate Model:
#> 
#> Data:
#>     dataset rows columns
#>   line_list 1502       4
#>  population    3       2
#> 
#> Detection Probability Priors:
#>   active prior: beta(1.0, 1.0)
#>   passive_asymptomatic prior: beta(1.0, 3.0)
#>   passive_symptomatic prior: beta(3.0, 1.0)
#> 
#> Timesteps:
#>   time: 2024-01-01 to 2024-03-25 (13 timesteps)
#> 
#> Detection:
#>   column: detection
#>     active: 350 cases (values: Active)
#>     passive: 1152 cases (values: Passive)
#> 
#> Outcome:
#>   column: outcome
#>     asymptomatic: 154 cases (values: Asymptomatic)
#>     symptomatic: 790 cases (values: Symptomatic)
#>     severe: 558 cases (values: Death)
#> 
#> Strata:
#>   age: 3 levels, df = 0 (adult, senior, youth)

estimate <- fit(
  model,
  chains = 1L,
  iter = 250L,
  seed = 123L,
  refresh = 0
)
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(estimate)
#> Detection Rates:
#>                      Estimate
#> passive_asymptomatic  0.05704
#> passive_symptomatic   0.95175
#> active                0.15974
#> 
#> Severity Estimates:
#>     age IFR Estimate SIR Estimate
#>   adult       0.3097       0.6554
#>  senior       0.3005       0.7675
#>   youth       0.2400       0.5198
```

If you need more control, you can still start from
[`SeverityEstimateModel()`](https://accidda.github.io/SeverityEstimate/reference/SeverityEstimateModel.md)
and use
[`set_strata()`](https://accidda.github.io/SeverityEstimate/reference/strata.md),
[`set_timesteps()`](https://accidda.github.io/SeverityEstimate/reference/timesteps.md),
[`set_detection()`](https://accidda.github.io/SeverityEstimate/reference/detection.md),
[`set_outcome()`](https://accidda.github.io/SeverityEstimate/reference/outcome.md),
and the prior setters directly. That is useful, for example, when you
want a smoothed ordered strata effect with `degrees_of_freedom > 0L`
rather than the default unsmoothed categorical effect.

## Extracting results

### Detection rate parameters

[`calculate_parameter_estimates()`](https://accidda.github.io/SeverityEstimate/reference/calculate_parameter_estimates.md)
returns a summary of the three detection rate parameters fitted by the
model: the active detection rate, the passive asymptomatic detection
rate, and the passive symptomatic detection rate. The `alpha` argument
controls which credible interval bounds are returned.

``` r

calculate_parameter_estimates(estimate, alpha = 0.05)
#>                        parameter                      parameter_description
#> 1               active_detection                      active detection rate
#> 2 passive_asymptomatic_detection mildly/asymptomatic passive detection rate
#> 3  passive_symptomatic_detection     severe symptoms passive detection rate
#>   mean_estimate median_estimate  lower_05   upper_05
#> 1    0.15974402      0.15968138 0.1385260 0.18011611
#> 2    0.05703974      0.05604461 0.0369103 0.08813589
#> 3    0.95174877      0.96341735 0.8526623 0.99385044
```

### Fatality ratios

[`calculate_fatality_ratio()`](https://accidda.github.io/SeverityEstimate/reference/calculate_fatality_ratio.md)
returns IFR and SIR estimates broken down by stratum. Setting
`naive_estimate = TRUE` also includes a naive observed ratio computed
directly from the raw counts, which provides a useful point of
comparison against the model-adjusted estimates.

``` r

calculate_fatality_ratio(
  estimate, median_estimate = FALSE, naive_estimate = TRUE
)
#>      age ifr_mean_estimate ifr_lower_05 ifr_upper_05 sir_mean_estimate
#> 1  adult         0.3096621    0.2777563    0.3424366         0.6553673
#> 2 senior         0.3005075    0.2438216    0.3661525         0.7675303
#> 3  youth         0.2400402    0.1994504    0.2927146         0.5197518
#>   sir_lower_05 sir_upper_05 naive_ifr naive_sir
#> 1    0.6052815    0.7026613 0.3890135 0.9047085
#> 2    0.6653631    0.8383285 0.3495935 0.9471545
#> 3    0.4442299    0.5791658 0.3434066 0.8461538
```
