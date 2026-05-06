# Getting Started

## Overview

`SeverityEstimate` fits a Bayesian model to estimate the infection
fatality ratio (IFR) and symptomatic infection rate (SIR) from case line
list data, accounting for the fact that active and passive surveillance
systems detect cases at different rates and with different symptom
profiles. The model is based on [Lessler et
al. (2016)](https://doi.org/10.1093/aje/kwv452).

The package uses a pipeline API similar in spirit to `tidymodels`: you
first build and configure a `SeverityEstimateModel` object, then call
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
to run the Stan sampler and obtain a `SeverityEstimateFit` from which
estimates are extracted.

``` r
options(mc.cores = 1L)
library(SeverityEstimate)
#> Loading required package: checkmate
#> Loading required package: fs
#> Loading required package: jinjar
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
#>       patient          time                   age           detection   
#>  Length   :1502   Min.   :2024-01-01   Length   :1502   Length   :1502  
#>  N.unique :1502   1st Qu.:2024-01-22   N.unique :   3   N.unique :   2  
#>  N.blank  :   0   Median :2024-02-12   N.blank  :   0   N.blank  :   0  
#>  Min.nchar:   4   Mean   :2024-02-12   Min.nchar:   5   Min.nchar:   6  
#>  Max.nchar:   7   3rd Qu.:2024-03-04   Max.nchar:   6   Max.nchar:   7  
#>                   Max.   :2024-03-25                                    
#>       outcome    
#>  Length   :1502  
#>  N.unique :   3  
#>  N.blank  :   0  
#>  Min.nchar:   5  
#>  Max.nchar:  12  
#> 
```

## Building and fitting the model

The modelling workflow has two stages. First, a `SeverityEstimateModel`
is constructed by specifying the line list, the population table, and
the model configuration. Each `set_*` call adds one piece of
configuration and returns the model so calls can be chained.

- `set_active_prior/passive_asymptomatic_prior/passive_symptomatic_prior`:
  Set Beta distribution priors for each surveillance detection rate.
  Here we use a weakly informative prior for active detection and priors
  that reflect the expectation that passive surveillance is more likely
  to capture symptomatic than asymptomatic cases.
- `set_strata`: Declares the column used for stratification and its
  ordered levels. Levels should be provided explicitly to ensure a
  consistent ordering in the output.
- `set_timesteps`: Declares the column that identifies the time period
  of each observation.
- `set_detection`: Maps the values in the detection column to the
  canonical types `active` and `passive` expected by the model.
- `set_outcome`: Maps the values in the outcome column to the canonical
  types `asymptomatic`, `symptomatic`, and `severe` expected by the
  model.

Once configured,
[`fit()`](https://accidda.github.io/SeverityEstimate/reference/fit.md)
compiles the Stan model and runs the sampler, passing any additional
arguments (e.g. `chains`, `iter`) through to
[`rstan::stan()`](https://mc-stan.org/rstan/reference/stan.html). The
settings below are intentionally small to keep this vignette reasonably
quick to render. They are not sufficient to assess convergence or
support serious inference. In practice you’d want to use more chains and
iterations and review the usual Stan diagnostics.

``` r
population <- strata[, c("age", "population")]

model <- SeverityEstimateModel(linelist, population) |>
  set_active_prior(alpha = 1.0, beta = 1.0) |>
  set_passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
  set_passive_symptomatic_prior(alpha = 3.0, beta = 1.0) |>
  set_strata(
    "age",
    levels = strata[, "age"],
    degrees_of_freedom = 1L
  ) |>
  set_timesteps("time") |>
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
  chains = 1L,
  iter = 500L,
  seed = 123L,
  refresh = 0
)
#> Warning: There were 245 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
```

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
#>   mean_estimate median_estimate   lower_05   upper_05
#> 1    0.15821158       0.1584723 0.13539286 0.17974349
#> 2    0.05907498       0.0578289 0.03350709 0.09356547
#> 3    0.93582168       0.9476105 0.81435766 0.99826088
```

### Fatality ratios

[`calculate_fatality_ratio()`](https://accidda.github.io/SeverityEstimate/reference/calculate_fatality_ratio.md)
returns IFR and SIR estimates broken down by stratum. Setting
`naive_estimate = TRUE` also includes a naive observed ratio computed
directly from the raw counts, which provides a useful point of
comparison against the model-adjusted estimates.

``` r
ratios <- calculate_fatality_ratio(
  estimate, median_estimate = FALSE, naive_estimate = TRUE
)
ratios[match(c("youth", "adult", "senior"), ratios$age), ]
#>      age ifr_mean_estimate ifr_lower_05 ifr_upper_05 sir_mean_estimate
#> 3  youth         0.2396614    0.2004706    0.2877567         0.5252368
#> 1  adult         0.3080786    0.2750720    0.3446976         0.6564920
#> 2 senior         0.2998994    0.2467768    0.3706342         0.7746898
#>   sir_lower_05 sir_upper_05 naive_ifr naive_sir
#> 3    0.4539460    0.5939186 0.3434066 0.8461538
#> 1    0.6027168    0.7151124 0.3890135 0.9047085
#> 2    0.6781886    0.8666125 0.3495935 0.9471545
```
