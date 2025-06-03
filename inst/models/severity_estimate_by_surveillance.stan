data {
  // *Dimensions of data*
  // The number of strata
  int <lower=0> strata_groups;
  // The number of times
  int <lower=0> time_groups;
  // The number of cases observed through active surveillance
  int <lower=0> observed_active;
  // The number of cases observed through passive surveillance
  int <lower=0> observed_passive;

  // *Matrices of data*
  // The number of incidence detected through active surveillance
  int <lower=0> I_active[time_groups, strata_groups];
  // The number of incidence detected through passive surveillance
  int <lower=0> I_passive[time_groups, strata_groups];
  // The total population
  int <lower=0> population[strata_groups];

  // *Vectors of data*
  // The strata number of the actively observed cases
  int <lower=0> strata_active[observed_active];
  // Indicator if the actively observed case presented with symptoms
  int <lower=0> symptoms_active[observed_active];
  // Indicator if the actively observed case died
  int <lower=0> dead_active[observed_active];
  // The strata number of the passively observed cases
  int <lower=0> strata_passive[observed_passive];
  // Indicator if the passively observed case presented with symptoms
  int <lower=0> symptoms_passive[observed_passive];
  // Indicator if the passively observed case died
  int <lower=0> dead_passive[observed_passive];

  // *Model parameters and priors*
  // The stdev of the community hazard brownian motion
  real <lower=0> hazard_std;
  // Spline degrees of freedom for mortality and symptom terms
  int <lower=1> degrees_of_freedom;
  // Active detection probability prior
  real <lower=0> active_detection_alpha;
  real <lower=0> active_detection_beta;
  // Passive detection probability prior
  real <lower=0> passive_asymptomatic_alpha;
  real <lower=0> passive_asymptomatic_beta;
  real <lower=0> passive_symptomatic_alpha;
  real <lower=0> passive_symptomatic_beta;
}

parameters {
   // Symtom development/mortality spline coefficients
  real alpha[1 + degrees_of_freedom];
  real mort_coef[1 + degrees_of_freedom];
  // The hazard of infection in each time step
  real logit_hzd[time_groups, strata_groups];
  // Active detection probability
  real <lower=0, upper=1> active_detection;
  // Passive detection probabilities
  real <lower=0, upper=1> passive_asymptomatic_detection;
  real <lower=0, upper=1> passive_symptomatic_detection;
}

transformed parameters {
  // *Transformed parameters*
  // Strata specific symptom/mortality rate
  real <lower=0, upper=1> xi[strata_groups];
  real <lower=0, upper=1> mortality[strata_groups];
  // The susceptibles/casesat at each time by strata
  real <lower=0> S[time_groups, strata_groups];
  real <lower=0> C[time_groups, strata_groups];
  // Intermediates
  real xi_tmp;
  real mort_tmp;

  for (i in 1:strata_groups) {
    // Calculate the symptomatic/mortality rate from the spline
    xi_tmp = alpha[1];
    mort_tmp = mort_coef[1];
    for (j in 1:degrees_of_freedom) {
      xi_tmp = xi_tmp + (alpha[j + 1] * pow(i - 1, j));
      mort_tmp = mort_tmp + (mort_coef[j + 1] * pow(i - 1, j));
    }
    xi[i] = inv_logit(xi_tmp);
    mortality[i] = inv_logit(mort_tmp);

    // For first time step assume the population at risk is the full population
    S[1, i] = population[i];
    C[1, i] = population[i] * inv_logit(logit_hzd[1, i]);
  }

  // For each subsequent time step we assume the number of passive cases is
  // based on the passive hazard and the number susceptible
  for (i in 2:time_groups) {
    for (j in 1:strata_groups) {
      S[i, j] = S[i - 1, j] - C[i - 1, j];
      C[i, j] = S[i, j] * inv_logit(logit_hzd[i, j]);
    }
  }
}

model {
  // *Model priors*
  // Spline coefficients
  for (i in 1:(degrees_of_freedom + 1)) {
    alpha[i] ~ normal(0, 10000);
    mort_coef[i] ~ normal(0, 10000);
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
        logit((1.0 * I_passive[i, j])/population[j]
          + (100.0 * machine_precision())),
        hazard_std
      );
      I_active[i, j] ~ poisson(active_detection * C[i, j]);
      I_passive[i, j] ~ poisson(
        (1-active_detection) * (passive_asymptomatic_detection * (1 - xi[j])
         + passive_symptomatic_detection * xi[j]) * C[i, j]
      );
    }
  }

  // Symtpomatic proability in active cases
  for (i in 1:observed_active) {
    symptoms_active[i] ~ bernoulli(xi[strata_active[i]]);
    dead_active[i] ~ bernoulli(mortality[strata_active[i]]);
  }

  // Reporting delay in active cases
  for (i in 1:observed_passive) {
    symptoms_passive[i] ~ bernoulli(
      (1 -
        ((1 - (passive_symptomatic_detection * xi[strata_passive[i]]))
          * (1 - mortality[strata_passive[i]])))/
				(1 - (1 - mortality[strata_passive[i]])
				  * (1 - ((1 - xi[strata_passive[i]])
				    * passive_asymptomatic_detection + xi[strata_passive[i]]
				     * passive_symptomatic_detection)))
		);
		dead_passive[i] ~ bernoulli(
		  mortality[strata_passive[i]]/
		    (1 - (
		      (1 - mortality[strata_passive[i]]) *
		        (1 -((1 - xi[strata_passive[i]]) *
		          passive_asymptomatic_detection + xi[strata_passive[i]]
		           * passive_symptomatic_detection))))
		);
  }
}

generated quantities {
  // *Additional helpful quantities*
  // Distribution of unseen additional active/passive cases
  int <lower=0> C_active_additional[time_groups, strata_groups];
  int <lower=0> C_passive_additional[time_groups, strata_groups];
  real gq_tmp;

  for (i in 1:time_groups) {
    for (j in 1:strata_groups) {
      // First generate the active cases, used in passive cases
      gq_tmp = C[i, j] - I_active[i, j];
      if (gq_tmp > 0) {
        C_active_additional[i, j] = poisson_rng(active_detection * gq_tmp);
      } else {
        C_active_additional[i, j] = 0;
      }
      // Next generate the passive cases
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
