data {
    // Prior specifications
    array[2] real<lower=0.0> active;
    array[2] real<lower=0.0> passive_asymptomatic;
    array[2] real<lower=0.0> passive_symptomatic;
    // Time data
    int <lower=0> n_time;
    // Line list data
    int <lower=0> ll_time_index;
}

parameters {
    // Detection parameters
    real <lower=0, upper=1> active_detection;
    real <lower=0, upper=1> passive_asymptomatic_detection;
    real <lower=0, upper=1> passive_symptomatic_detection;
}

model {
    // Detection rate prior distributions
    active_detection ~ beta(active[0], active[1]);
    passive_asymptomatic_detection ~ beta(
        passive_asymptomatic[0],
        passive_asymptomatic[1]
    );
    passive_symptomatic_detection ~ beta(
        passive_symptomatic[0],
        passive_symptomatic[1]
    );
}
