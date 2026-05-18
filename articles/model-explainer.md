# Model Explainer

## Overview

`SeverityEstimate` fits a Bayesian model for the symptomatic infection
rate (SIR) and infection fatality ratio (IFR) using case line list data
observed through active and passive surveillance. The package is based
on [Lessler et al. (2016)](https://doi.org/10.1093/aje/kwv452), and the
supplementary material distributed with that paper gives the core
likelihood derivations.

This vignette is about the model itself rather than the workflow. It has
two goals:

1.  Restate the key probability model in the notation used by the
    package.
2.  Call out the implementation details that matter in
    `inst/stan/estimate_severity.stan`, especially strata handling and
    the latent hazard process.

For a package walkthrough, see
[`vignette("getting-started")`](https://accidda.github.io/SeverityEstimate/articles/getting-started.md).

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

## Notation

The Stan program indexes time by $`t = 1, \dots, T`$ and strata cells by
$`g = 1, \dots, G`$.

For each stratum $`g`$, the model estimates:

- `xi[g]` or $`\xi_g`$: The symptomatic infection rate (SIR), that is
  the probability an infection is symptomatic.
- `mortality[g]` or $`\text{IFR}_g`$: The infection fatality ratio
  (IFR), that is the probability an infection dies.

The surveillance parameters are shared across strata:

- `active_detection` or $`\phi`$: Probability an infection is detected
  by active surveillance.
- `passive_asymptomatic_detection` or $`\psi_1`$: Probability an
  asymptomatic infection is eventually detected by passive surveillance.
- `passive_symptomatic_detection` or $`\psi_2`$: Probability a
  symptomatic infection is eventually detected by passive surveillance.

## Occurrence of Symptoms Among Confirmed Infections

As in the paper, this model assumes that the SIR/IFR detected through
active surveillance is the same rate as all infections, regardless of
detection status:

``` math
\Pr(\text{Symptomatic} \mid g, \text{Active surveillance}) = \xi_g,
```

``` math
\Pr(\text{Death} \mid g, \text{Active surveillance}) = \text{IFR}_g.
```

Then for convenience, the stan model defines an intermediate quantity
$`\theta_g`$ as:

``` math
\theta_g = \psi_1(1 - \xi_g) + \psi_2\xi_g.
```

Then, to account for the fact that the vast majority of cases detected
through passive surveillance will be more severe cases, the SIR/IFR
conditional on being detected through passive surveillance is given by:

``` math
\Pr(\text{Symptomatic} \mid g, \text{Passive surveillance}) = \frac{1 - (1 - \text{IFR}_g)(1 - \psi_2\xi_g)}{1 - (1 - \text{IFR}_g)(1 - \theta_g)},
```

``` math
\Pr(\text{Death} \mid g, \text{Passive surveillance}) = \frac{\text{IFR}_g}{1 - (1 - \text{IFR}_g)(1 - \theta_g)}.
```

The presentation of individual cases, by severity, stratum, and
surveillance method can be estimated via a bernoulli likelihood with the
probabilities above.

## Latent Infections and Observed Incidence

- `population[g]` or $`P_g`$: The total number of persons in stratum
  $`g`$ which is used to initilize the model populations.
- `S[t, g]` or $`S_{t,g}`$: The number of susceptibles at time $`t`$ in
  stratum $`g`$.
- `C[t, g]` or $`C_{t,g}`$: The number of latent infections at time
  $`t`$ in stratum $`g`$.
- `logit_hzd[t, g]` or $`\lambda_{t,g}`$: The force of infection at time
  $`t`$ in stratum $`g`$ on a logit scale.

The susceptible population, latent infections, and force of infection at
$`t=1`$ are intialized via:

``` math
S_{1,g} = P_g,
\qquad
C_{1,g} = P_g \operatorname{logit}^{-1}(\lambda_{1,g}).
```

and for $`t = 2, \dots, T`$,

``` math
S_{t,g} = S_{t-1,g} - C_{t-1,g},
\qquad
C_{t,g} = S_{t,g} \operatorname{logit}^{-1}(\lambda_{t,g}),
```

Observed counts are linked to these latent infections by Poisson
observation models:

``` math
I_{\text{Active surveillance},t,g} \sim \operatorname{Poisson}(\phi C_{t,g}),
```

``` math
I_{\text{Passive surveillance},t,g} \sim \operatorname{Poisson}((1-\phi)\theta_g C_{t,g}).
```

### Force of Infection Estimation

For simplicity it’s assumed that the force of infection follows the
trajectory of passively of passively observed latent infections:

``` math
\lambda_{t,g} \sim \operatorname{Normal}\left(\operatorname{logit}\left(\frac{I_{\text{Passive surveillance},t,g}}{P_g}\right),\sigma_h\right),
```

where `hazard_std` or $`\sigma_h`$ is set to a constant 3.

## Strata Effects

The paper formulation is age-specific, but the package generalizes this
to an arbitrary cross-product of strata variables. The key change is
that the Stan model does not estimate a completely free parameter for
every strata cell. Instead, it builds additive linear predictors and
then transforms them via a logistic link function:

``` math
\xi_g = \operatorname{logit}^{-1}(\mu_{\xi} + X \beta_{\xi}),
```

``` math
\text{IFR}_g = \operatorname{logit}^{-1}(\mu_\text{mort} + X \beta_\text{mort}).
```

Where `X_strata` or $`X`$ is a design matrix.

### Unsmoothed strata

If a strata variable is declared with `degrees_of_freedom = 0L`, the
package uses a sum-to-zero categorical contrast basis with `K - 1`
columns for `K` levels. This gives a standard categorical effect while
keeping the intercept interpretable as a grand mean on the logit scale.

### Ordered, smoothed strata

If `degrees_of_freedom > 0L`, the strata levels must be supplied
explicitly and are treated as ordered. The package then uses orthogonal
polynomial basis terms via
[`stats::poly()`](https://rdrr.io/r/stats/poly.html) and standardizes
the resulting columns before passing them to Stan.

If there are `K` ordered levels and `d` requested degrees of freedom,
the basis has `d` columns with `d <= K - 2`. For example, a five-level
age variable with `degrees_of_freedom = 2L` produces two basis columns
corresponding roughly to a linear and quadratic trend across the ordered
levels.

### Multiple strata dimensions

When multiple strata variables are declared, their basis blocks are
concatenated side by side. This makes the model additive across strata
dimensions:

``` math
\eta_g = \mu + f_{\text{age}}(g) + f_{\text{region}}(g) + f_{\text{risk}}(g)
+ \cdots
```

There are no interaction terms unless the user encodes them manually as
an additional strata variable.

## Priors and generated quantities

The remaining priors are straightforward:

- $`\mu_\xi`$ and $`\mu_\text{mort}`$ are drawn from a
  $`\operatorname{Normal}(0, 2)`$ prior.
- The strata coefficients $`\beta_\xi`$ and $`\beta_\text{mort}`$ also
  have $`\operatorname{Normal}(0, 2)`$ priors when any strata basis
  columns are present.
- The three detection probabilities have user-configurable Beta priors
  which default to $`\operatorname{Beta}(1, 1)`$ if not explicitly set
  by the user.

The generated-quantities block simulates additional unobserved cases
through active and passive surveillance. Those draws are useful when
downstream code wants posterior samples of total infections rather than
only the observed case counts.

## Compact worked example

The code below mirrors the workflow from
[`vignette("getting-started")`](https://accidda.github.io/SeverityEstimate/articles/getting-started.md),
but the main point here is how the fitted object maps back to the model
quantities just described. This example also includes an additional
`health_occupation` strata to highlight the ability to fit unordered
strata as well.

To encode extra infection risk for health care professionals, the
synthetic data uses a higher force of infection for the
`health_occupation = "yes"` strata. That change belongs in the hazard
process, not in `sir`, because `sir` controls symptom risk conditional
on infection rather than exposure risk.

``` r

strata <- do.call(
  rbind,
  lapply(
    list(
      list(
        age = "youth",
        health_occupation = "no",
        ifr = 0.25,
        sir = 0.40,
        population = 10000L
      ),
      list(
        age = "adult",
        health_occupation = "no",
        ifr = 0.30,
        sir = 0.55,
        population = 20000L
      ),
      list(
        age = "senior",
        health_occupation = "no",
        ifr = 0.35,
        sir = 0.70,
        population = 5000L
      ),
      list(
        age = "youth",
        health_occupation = "yes",
        ifr = 0.25,
        sir = 0.50,
        population = 500L
      ),
      list(
        age = "adult",
        health_occupation = "yes",
        ifr = 0.30,
        sir = 0.65,
        population = 2000L
      ),
      list(
        age = "senior",
        health_occupation = "yes",
        ifr = 0.35,
        sir = 0.80,
        population = 250L
      )
    ),
    as.data.frame
  )
)
times <- seq(as.Date("2024-01-01"), as.Date("2024-03-31"), "+7 days")
baseline_force_of_infection <- ifelse(
  strata$health_occupation == "yes",
  0.010,
  0.005
)
force_of_infection <- matrix(
  data = rep(baseline_force_of_infection, each = length(times)),
  nrow = length(times),
  ncol = nrow(strata)
)
linelist <- create_sample_linelist(
  strata,
  times,
  0.15,
  0.05,
  0.95,
  force_of_infection = force_of_infection,
  seed = 123L
)
population <- strata[, c("age", "health_occupation", "population")]
```

``` r

model <- SeverityEstimateModel(linelist, population) |>
  set_active_prior(alpha = 1.0, beta = 1.0) |>
  set_passive_asymptomatic_prior(alpha = 1.0, beta = 3.0) |>
  set_passive_symptomatic_prior(alpha = 3.0, beta = 1.0) |>
  set_strata(
    "age",
    levels = c("youth", "adult", "senior"),
    degrees_of_freedom = 1L
  ) |>
  set_strata("health_occupation") |>
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
```

[`calculate_parameter_estimates()`](https://accidda.github.io/SeverityEstimate/reference/calculate_parameter_estimates.md)
summarizes the three surveillance detection parameters, while
[`calculate_fatality_ratio()`](https://accidda.github.io/SeverityEstimate/reference/calculate_fatality_ratio.md)
summarizes `mortality[g]` as IFR and `xi[g]` as SIR for each stratum.

``` r

calculate_parameter_estimates(estimate, alpha = 0.05)
#>                        parameter                      parameter_description
#> 1               active_detection                      active detection rate
#> 2 passive_asymptomatic_detection mildly/asymptomatic passive detection rate
#> 3  passive_symptomatic_detection     severe symptoms passive detection rate
#>   mean_estimate median_estimate   lower_05   upper_05
#> 1    0.16429132       0.1645077 0.14279436 0.18519107
#> 2    0.07024651       0.0709294 0.04860734 0.09187422
#> 3    0.95238435       0.9636443 0.84710890 0.99835436

calculate_fatality_ratio(
  estimate,
  median_estimate = TRUE,
  mean_estimate = FALSE,
  naive_estimate = TRUE
)
#>      age health_occupation ifr_median_estimate ifr_lower_05 ifr_upper_05
#> 1  youth                no           0.2869454    0.2492251    0.3342272
#> 2  youth               yes           0.2323607    0.1788415    0.3000969
#> 3  adult                no           0.3342020    0.3062570    0.3646334
#> 4  adult               yes           0.2760754    0.2115991    0.3461296
#> 5 senior                no           0.3807844    0.3357321    0.4480892
#> 6 senior               yes           0.3229806    0.2471775    0.4199844
#>   sir_median_estimate sir_lower_05 sir_upper_05 naive_ifr naive_sir
#> 1           0.4674317    0.4112720    0.5211716 0.4069401 0.8170347
#> 2           0.4664176    0.3284257    0.5926703 0.3500000 0.8750000
#> 3           0.6266265    0.5863260    0.6657964 0.4224806 0.8863049
#> 4           0.6161694    0.5203892    0.7385855 0.3231707 0.8597561
#> 5           0.7625821    0.6948667    0.8191488 0.4330709 0.9488189
#> 6           0.7485675    0.6654328    0.8550410 0.5555556 1.0000000
```

## What to keep in mind when reading fitted results

- IFR and SIR are stratum-specific but are linked through shared
  detection parameters.
- Passive cases are not sampled from the same outcome distribution as
  active cases, they are outcome biased..
- The current strata formulation is additive across strata dimensions
  unless the user constructs interaction strata explicitly.
- The current hazard prior is a per-cell normal prior on the logit
  hazard.

Those details are the main bridge between the original paper supplement
and the package’s current Stan implementation.
