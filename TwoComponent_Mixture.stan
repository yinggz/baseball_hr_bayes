data {
    int<lower=0> n; // Number of data points
    vector[n] Y; // Data
}
parameters {
    real<lower=0,upper=1> alpha; // Mixing proportion
    ordered[2] mu; // Means of the two components
    vector<lower=0>[2] sigma; // Standard deviations of the two components
}
model {
    alpha ~ beta(1, 1);
    mu ~ normal(0, 1);
    sigma ~ inv_gamma(2, 0.1);
    for (i in 1:n) {
        target += log_mix(alpha,
                          normal_lpdf(Y[i] | mu[1], sigma[1]),
                          normal_lpdf(Y[i] | mu[2], sigma[2]));
    }
}
