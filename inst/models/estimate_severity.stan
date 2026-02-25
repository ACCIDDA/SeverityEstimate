data {
    // User specified parameterization for detection probabilities
    real<lower=0> active_prior_alpha;
    real<lower=0> active_prior_beta;
    real<lower=0> passive_asymptomatic_prior_alpha;
    real<lower=0> passive_asymptomatic_prior_beta;
    real<lower=0> passive_symptomatic_prior_alpha;
    real<lower=0> passive_symptomatic_prior_beta;
}

parameters {
    // Detection probabilities
    real<lower=0, upper=1> active_detection;
    real<lower=0, upper=1> passive_asymptomatic_detection;
    real<lower=0, upper=1> passive_symptomatic_detection;
}

model {
    // Sample detection probabilities
    active_detection ~ beta(
        active_prior_alpha,
        active_prior_beta
    );
    passive_asymptomatic_detection ~ beta(
        passive_asymptomatic_prior_alpha,
        passive_asymptomatic_prior_beta
    );
    passive_symptomatic_detection ~ beta(
        passive_symptomatic_prior_beta,
        passive_symptomatic_prior_beta
    );
}
