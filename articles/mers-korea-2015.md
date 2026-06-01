# MERS Korea 2015

## Overview

This vignette shows how a public outbreak line list can be reshaped for
`SeverityEstimate`. The example uses the MERS-CoV outbreak in South
Korea in 2015 from the `outbreaks` package:
<https://www.reconverse.org/outbreaks/reference/mers_korea_2015.html>.

The data is not a perfect match for this package’s assumptions. In
particular, the line list does not contain a explicit active/passive
surveillance labels and the denominator for the outbreak was not the
full population of South Korea. Here we use the case-contact data as a
transparent surveillance proxy and use national age-specific population
denominators as a simple first pass.

``` r

options(mc.cores = 1L)
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

## Line List

The `mers_korea_2015` object contains a case line list and a table of
probable case-to-case transmission links.

``` r

mers <- outbreaks::mers_korea_2015
linelist_raw <- mers$linelist
contacts <- mers$contacts

str(linelist_raw)
#> 'data.frame':    162 obs. of  15 variables:
#>  $ id            : chr  "SK_1" "SK_2" "SK_3" "SK_4" ...
#>  $ age           : int  68 63 76 46 50 71 28 46 56 44 ...
#>  $ age_class     : chr  "60-69" "60-69" "70-79" "40-49" ...
#>  $ sex           : Factor w/ 2 levels "F","M": 2 1 2 1 2 2 1 1 2 2 ...
#>  $ place_infect  : Factor w/ 2 levels "Middle East",..: 1 2 2 2 2 2 2 2 2 2 ...
#>  $ reporting_ctry: Factor w/ 2 levels "China","South Korea": 2 2 2 2 2 2 2 2 2 1 ...
#>  $ loc_hosp      : Factor w/ 13 levels "365 Yeollin Clinic, Seoul",..: 10 10 10 10 1 10 10 13 10 10 ...
#>  $ dt_onset      : Date, format: "2015-05-11" "2015-05-18" ...
#>  $ dt_report     : Date, format: "2015-05-19" "2015-05-20" ...
#>  $ week_report   : Factor w/ 5 levels "2015_21","2015_22",..: 1 1 1 2 2 2 2 2 2 2 ...
#>  $ dt_start_exp  : Date, format: "2015-04-18" "2015-05-15" ...
#>  $ dt_end_exp    : Date, format: "2015-05-04" "2015-05-20" ...
#>  $ dt_diag       : Date, format: "2015-05-20" "2015-05-20" ...
#>  $ outcome       : Factor w/ 2 levels "Alive","Dead": 1 1 2 1 1 2 1 1 1 1 ...
#>  $ dt_death      : Date, format: NA NA ...
head(contacts)
#>    from     to       exposure diff_dt_onset
#> 1 SK_14 SK_113 Emergency room            10
#> 2 SK_14 SK_116 Emergency room            13
#> 3 SK_14  SK_41 Emergency room            14
#> 4 SK_14 SK_112 Emergency room            14
#> 5 SK_14 SK_100 Emergency room            15
#> 6 SK_14 SK_114 Emergency room            15
```

We use reporting week as the model time step because it is complete for
all cases.

The `outbreaks` line list reports final outcome as `Alive` or `Dead`.
For this model we collapse those to the package’s outcome labels:

- `Alive` becomes `Symptomatic`
- `Dead` becomes `Death`

This means the model is estimating fatality among observed symptomatic
MERS infections rather than an asymptomatic infection rate.

``` r

linelist <- data.frame(
  time = linelist_raw$week_report,
  age_class = as.character(linelist_raw$age_class),
  detection = ifelse(
    linelist_raw$id %in% contacts$to,
    "Active",
    "Passive"
  ),
  outcome = ifelse(linelist_raw$outcome == "Dead", "Death", "Symptomatic")
)

head(linelist)
#>      time age_class detection     outcome
#> 1 2015_21     60-69   Passive Symptomatic
#> 2 2015_21     60-69    Active Symptomatic
#> 3 2015_21     70-79    Active       Death
#> 4 2015_22     40-49    Active Symptomatic
#> 5 2015_22     50-59    Active Symptomatic
#> 6 2015_22     70-79    Active       Death
table(linelist$detection, linelist$outcome)
#>          
#>           Death Symptomatic
#>   Active     12          83
#>   Passive     7          60
```

The detection variable is a proxy. Cases appearing as `contacts$to` are
treated as `Active` because they are linked to a probable source case in
the outbreak investigation data. Cases without a recorded source are
treated as `Passive` because they are closer to unlinked or index
presentations. This is not the same as a validated surveillance field,
so fitted estimates should be interpreted as an exploratory analysis.

## Population

The MERS data already provide age classes, which makes age
stratification straightforward. As a simple denominator, we use Republic
of Korea 2015 population estimates from page 58 of the United Nations
Demographic Yearbook 2015, Table 7:
<https://unstats.un.org/unsd/demographic-social/products/dyb/documents/dyb2015/table07.pdf>.

The UN table reports standard five-year age groups. The population table
below aggregates those into the age classes used by the line list. The
youngest line list stratum is labelled `10-20`; here it is approximated
by `10-14 + 15-19`.

``` r

population <- data.frame(
  age_class = c(
    "10-20", "20-29", "30-39", "40-49",
    "50-59", "60-69", "70-79", "80-89"
  ),
  population = c(
    2475819L + 3175320L,  # 10-14 + 15-19
    3525734L + 3279000L,  # 20-24 + 25-29
    3807021L + 3846466L,  # 30-34 + 35-39
    4239971L + 4225823L,  # 40-44 + 45-49
    4255812L + 3857441L,  # 50-54 + 55-59
    2740743L + 2121186L,  # 60-64 + 65-69
    1721079L + 1371183L,  # 70-74 + 75-79
     859318L +  387911L     # 80-84 + 85-89
  )
)

population
#>   age_class population
#> 1     10-20    5651139
#> 2     20-29    6804734
#> 3     30-39    7653487
#> 4     40-49    8465794
#> 5     50-59    8113253
#> 6     60-69    4861929
#> 7     70-79    3092262
#> 8     80-89    1247229
```

These denominators are intentionally broad. A more specific analysis
would use the population at risk in exposed hospitals, health care
workers, visitors, patients, or monitored contacts rather than the
national resident population.

## Model

We build the model explicitly so that age is treated as an ordered,
smoothed strata. With eight age classes, `degrees_of_freedom = 2L` uses
a two-column polynomial basis for the age effect. Otherwise pretty broad
priors are used for the detection probabilities.

``` r

age_levels <- population$age_class

model <- SeverityEstimateModel(linelist, population) |>
  set_active_prior(alpha = 1.0, beta = 1.0) |>
  set_passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
  set_passive_symptomatic_prior(alpha = 3.0, beta = 1.0) |>
  set_timesteps("time") |>
  set_strata(
    "age_class",
    levels = age_levels,
    degrees_of_freedom = 2L
  ) |>
  set_detection(
    "detection",
    map = c("Active" = "active", "Passive" = "passive")
  ) |>
  set_outcome(
    "outcome",
    map = c("Symptomatic" = "symptomatic", "Death" = "severe")
  )
```

The sampler settings below are intentionally modest so that the vignette
remains reasonable to render. They are enough to exercise the workflow,
but they should be increased and checked with the usual Stan diagnostics
before interpreting the results.

``` r

estimate <- fit(
  model,
  chains = 2L,
  iter = 1000L,
  seed = 123L,
  cores = 2L,
  refresh = 0L
)

summary(estimate)
#> Detection Rates:
#>                      Estimate
#> passive_asymptomatic   0.2389
#> passive_symptomatic    0.9018
#> active                 0.5576
#> 
#> Severity Estimates:
#>  age_class IFR Estimate SIR Estimate
#>      10-20     0.008608       0.8793
#>      20-29     0.010872       0.9718
#>      30-39     0.020272       0.9878
#>      40-49     0.044542       0.9914
#>      50-59     0.092906       0.9931
#>      60-69     0.161750       0.9938
#>      70-79     0.233655       0.9916
#>      80-89     0.295652       0.9698
```

After fitting, the usual extractors can be used to inspect surveillance
parameters and age-stratified severity estimates:

``` r

calculate_parameter_estimates(estimate, alpha = 0.05)
#>                        parameter                      parameter_description
#> 1               active_detection                      active detection rate
#> 2 passive_asymptomatic_detection mildly/asymptomatic passive detection rate
#> 3  passive_symptomatic_detection     severe symptoms passive detection rate
#>   mean_estimate median_estimate    lower_05  upper_05
#> 1     0.5575758       0.5593180 0.463300782 0.6386171
#> 2     0.2389194       0.1940360 0.009461287 0.6888964
#> 3     0.9018130       0.9247272 0.666193445 0.9969771

calculate_fatality_ratio(
  estimate,
  median_estimate = FALSE,
  naive_estimate = TRUE
)
#>   age_class ifr_mean_estimate ifr_lower_05 ifr_upper_05 sir_mean_estimate
#> 1     10-20        0.00860840 6.084248e-06   0.06546689         0.8793125
#> 2     20-29        0.01087160 1.620682e-04   0.05448464         0.9717686
#> 3     30-39        0.02027186 2.312906e-03   0.06361522         0.9878425
#> 4     40-49        0.04454246 1.515555e-02   0.09594343         0.9913831
#> 5     50-59        0.09290600 4.663159e-02   0.15896195         0.9930823
#> 6     60-69        0.16174954 9.565683e-02   0.24825861         0.9938460
#> 7     70-79        0.23365456 1.374168e-01   0.34316453         0.9915631
#> 8     80-89        0.29565237 1.012433e-01   0.54555186         0.9697680
#>   sir_lower_05 sir_upper_05  naive_ifr naive_sir
#> 1    0.3227795    0.9999190 0.00000000         1
#> 2    0.8658067    0.9997115 0.00000000         1
#> 3    0.9469344    0.9997192 0.00000000         1
#> 4    0.9599605    0.9998622 0.03333333         1
#> 5    0.9706036    0.9998953 0.08823529         1
#> 6    0.9727306    0.9998416 0.18181818         1
#> 7    0.9624115    0.9998865 0.28000000         1
#> 8    0.7942806    0.9999810 0.22222222         1
```

## Interpretation

This example is useful as a real-world formatting workflow, but the
assumptions are stronger than in an ideal `SeverityEstimate` analysis:

- `Active` means contact-linked in the public outbreak data, not a
  confirmed active surveillance process.
- `Passive` means unlinked or index-like in the public outbreak data,
  not necessarily passively detected in the field.
- `Alive` cases are treated as symptomatic non-fatal cases, so the model
  is not learning asymptomatic infection risk from this data set.
- The national age-specific population is used as a pragmatic
  denominator even though the outbreak risk was concentrated in health
  care settings.

Those limitations should be kept with any estimates produced from this
example.
