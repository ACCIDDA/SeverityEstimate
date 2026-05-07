data {
  // *Dimensions of data*
  // The number of strata cells (cross-product of all strata levels)
  int<lower=1> strata_groups;
  // The number of times
  int<lower=1> time_groups;
  // The number of strata dimensions
  int<lower=0> n_strata_dims;
  // The number of levels within each strata dimension
  array[n_strata_dims] int<lower=1> strata_n_levels;
  // One-based start index for each strata dimension in the flattened effect vectors
  array[n_strata_dims] int<lower=1> strata_level_start;
  // The total number of strata levels across all dimensions
  int<lower=0> n_strata_levels_total;
  // The number of cases observed through active surveillance
  int<lower=0> observed_active;
  // The number of cases observed through passive surveillance
  int<lower=0> observed_passive;
  // *Matrices of data*
  // The number of incidence detected through active surveillance
  array[time_groups, strata_groups] int<lower=0> I_active;
  // The number of incidence detected through passive surveillance
  array[time_groups, strata_groups] int<lower=0> I_passive;
  // The total population
  array[strata_groups] int<lower=0> population;
  // *Vectors of data*
  // The strata cell index of the actively observed cases
  array[observed_active] int<lower=1> strata_active;
  // Indicator if the actively observed case presented with symptoms
  array[observed_active] int<lower=0, upper=1> symptoms_active;
  // Indicator if the actively observed case died
  array[observed_active] int<lower=0, upper=1> dead_active;
  // The strata cell index of the passively observed cases
  array[observed_passive] int<lower=1> strata_passive;
  // Indicator if the passively observed case presented with symptoms
  array[observed_passive] int<lower=0, upper=1> symptoms_passive;
  // Indicator if the passively observed case died
  array[observed_passive] int<lower=0, upper=1> dead_passive;
  // For each strata cell, the level index within each strata dimension
  array[strata_groups, n_strata_dims] int<lower=1> strata_index;
  // *Model parameters and priors*
  // The stdev of the community hazard brownian motion
  real<lower=0> hazard_std;
  // Active detection probability prior
  real<lower=0> active_detection_alpha;
  real<lower=0> active_detection_beta;
  // Passive detection probability prior
  real<lower=0> passive_asymptomatic_alpha;
  real<lower=0> passive_asymptomatic_beta;
  real<lower=0> passive_symptomatic_alpha;
  real<lower=0> passive_symptomatic_beta;
}
parameters {
  // Global intercepts for symptom and mortality rates
  real mu_xi;
  real mu_mort;
  // Flattened per-level fixed effects across all strata dimensions
  vector[n_strata_levels_total] alpha_xi;
  vector[n_strata_levels_total] alpha_mort;
  // The hazard of infection in each time step
  array[time_groups, strata_groups] real logit_hzd;
  // Active detection probability
  real<lower=0, upper=1> active_detection;
  // Passive detection probabilities
  real<lower=0, upper=1> passive_asymptomatic_detection;
  real<lower=0, upper=1> passive_symptomatic_detection;
}
transformed parameters {
  // Strata cell symptom/mortality rates
  array[strata_groups] real<lower=0, upper=1> xi;
  array[strata_groups] real<lower=0, upper=1> mortality;
  // The susceptibles/cases at each time by strata
  array[time_groups, strata_groups] real<lower=0> S;
  array[time_groups, strata_groups] real<lower=0> C;
  // Intermediates
  array[strata_groups] real<lower=0> theta;
  array[strata_groups] real<lower=0> passive_denom;
  real xi_tmp;
  real mort_tmp;

  // Calculate xi/mortality for each strata cell
  for (i in 1:strata_groups) {
    xi_tmp = mu_xi;
    mort_tmp = mu_mort;

    if (n_strata_dims > 0) {
      for (k in 1:n_strata_dims) {
        int effect_index = strata_level_start[k] + strata_index[i, k] - 1;
        xi_tmp += alpha_xi[effect_index];
        mort_tmp += alpha_mort[effect_index];
      }
    }

    xi[i] = inv_logit(xi_tmp);
    mortality[i] = inv_logit(mort_tmp);
    // For first time step assume the population at risk is the full population
    S[1, i] = population[i];
    C[1, i] = population[i] * inv_logit(logit_hzd[1, i]);
  }

  // For each subsequent time step, susceptibles deplete by prior cases
  for (i in 2:time_groups) {
    for (j in 1:strata_groups) {
      S[i, j] = S[i - 1, j] - C[i - 1, j];
      C[i, j] = S[i, j] * inv_logit(logit_hzd[i, j]);
    }
  }

  // Commonly reused detection-weighted quantities
  for (i in 1:strata_groups) {
    theta[i] = (passive_asymptomatic_detection * (1.0 - xi[i]))
      + (passive_symptomatic_detection * xi[i]);
    passive_denom[i] = 1.0 - ((1.0 - mortality[i]) * (1.0 - theta[i]));
  }
}
model {
  // *Model priors*
  // Global intercepts
  mu_xi ~ normal(0, 2);
  mu_mort ~ normal(0, 2);

  // Per-level fixed effects with a per-dimension sum-to-zero soft constraint
  if (n_strata_dims > 0) {
    for (k in 1:n_strata_dims) {
      segment(alpha_xi, strata_level_start[k], strata_n_levels[k]) ~ normal(0, 2);
      sum(segment(alpha_xi, strata_level_start[k], strata_n_levels[k]))
        ~ normal(0, 0.001);
      segment(alpha_mort, strata_level_start[k], strata_n_levels[k]) ~ normal(0, 2);
      sum(segment(alpha_mort, strata_level_start[k], strata_n_levels[k]))
        ~ normal(0, 0.001);
    }
  }

  // Priors for detection probabilities
  active_detection ~ beta(active_detection_alpha, active_detection_beta);
  passive_asymptomatic_detection ~ beta(
    passive_asymptomatic_alpha, passive_asymptomatic_beta
  );
  passive_symptomatic_detection ~ beta(
    passive_symptomatic_alpha, passive_symptomatic_beta
  );

  // Prior for community hazard
  for (i in 1:time_groups) {
    for (j in 1:strata_groups) {
      logit_hzd[i, j] ~ normal(
        logit((1.0 * I_passive[i, j]) / population[j]
          + (100.0 * machine_precision())),
        hazard_std
      );
      I_active[i, j] ~ poisson(active_detection * C[i, j]);
      I_passive[i, j] ~ poisson((1.0 - active_detection) * theta[j] * C[i, j]);
    }
  }

  // Symptomatic probability in active cases
  for (i in 1:observed_active) {
    symptoms_active[i] ~ bernoulli(xi[strata_active[i]]);
    dead_active[i] ~ bernoulli(mortality[strata_active[i]]);
  }

  // Symptomatic/mortality probability in passive cases (conditioned on detection)
  for (i in 1:observed_passive) {
    symptoms_passive[i] ~ bernoulli(
      (1.0 - ((1.0 - mortality[strata_passive[i]]))
        * (1.0 - (passive_symptomatic_detection * xi[strata_passive[i]])))
          / passive_denom[strata_passive[i]]
    );
    dead_passive[i] ~ bernoulli(
      mortality[strata_passive[i]] / passive_denom[strata_passive[i]]
    );
  }
}
generated quantities {
  // Distribution of unseen additional active/passive cases
  array[time_groups, strata_groups] int<lower=0> C_active_additional;
  array[time_groups, strata_groups] int<lower=0> C_passive_additional;
  real gq_tmp;

  for (i in 1:time_groups) {
    for (j in 1:strata_groups) {
      gq_tmp = C[i, j] - I_active[i, j];
      if (gq_tmp > 0) {
        C_active_additional[i, j] = poisson_rng(active_detection * gq_tmp);
      } else {
        C_active_additional[i, j] = 0;
      }

      gq_tmp = C[i, j] - I_passive[i, j] - I_active[i, j]
        - C_active_additional[i, j];
      if (gq_tmp > 0) {
        C_passive_additional[i, j] = poisson_rng(gq_tmp);
      } else {
        C_passive_additional[i, j] = 0;
      }
    }
  }
}
