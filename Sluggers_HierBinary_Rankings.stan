data { 
  int<lower=0> N;              // items 
  int<lower=0> K[N];           // initial trials 
  int<lower=0> y[N];           // initial successes 
 
  int<lower=0> K_new[N];       // new trials 
  int<lower=0> y_new[N];       // new successes 
} 
transformed data { 
  real min_y;   // minimum successes 
  real max_y;   // maximum successes 
  real mean_y;  // sample mean successes 
  real sd_y;    // sample std dev successes 
 
  min_y = min(y); 
  max_y = max(y); 
  mean_y = mean(to_vector(y)); 
  sd_y = sd(to_vector(y)); 
} 
parameters { 
  real<lower=0, upper=1> phi;         // population chance of success 
  real<lower=1> kappa;                // population concentration 
  vector<lower=0, upper=1>[N] theta;  // chance of success  
} 
model { 
  kappa ~ pareto(1, 1.5);                        // hyperprior 
  theta ~ beta(phi * kappa, (1 - phi) * kappa);  // prior 
  y ~ binomial(K, theta);                        // likelihood 
} 

generated quantities { 
  real log_p_new;     // posterior predictive log density remaining trials 
 
  int<lower=0> z[N];  // posterior prediction remaining trials 
 
  int<lower=1, upper=N> rnk[N];      // rank of player n 
  int<lower=0, upper=1> is_best[N];  // Pr[player n highest chance of success] 
 
  int<lower=0> y_rep[N];      // replications for existing items 
  int<lower=0> y_pop_rep[N];  // replications for simulated items 
 
  real<lower=0> min_y_rep;   // posterior predictive min replicated successes
  real<lower=0> max_y_rep;   // posterior predictive max replicated successes
  real<lower=0> mean_y_rep;  // posterior predictive sample mean replicated successes
  real<lower=0> sd_y_rep;    // posterior predictive sample std dev replicated successes
  
  int<lower=0, upper=1> p_min;  // posterior predictive p-values
  int<lower=0, upper=1> p_max;
  int<lower=0, upper=1> p_mean;
  int<lower=0, upper=1> p_sd;
 
  vector[N] log_p_news;  // posterior predictive log density for item

  for (n in 1:N)
  log_p_news[n] = binomial_lpmf(y_new[n]| K_new[n], theta[n]);
  log_p_new = sum(log_p_news);
  
  for (n in 1:N) 
    z[n] = binomial_rng(K_new[n], theta[n]); 
 
  { 
    int dsc[N]; 
    dsc = sort_indices_desc(theta); 
    for (n in 1:N) 
      rnk[dsc[n]] = n; 
  }
  
  for (n in 1:N) 
    is_best[n] = (rnk[n] == 1); 
 
  for (n in 1:N) 
    y_rep[n] = binomial_rng(K[n], theta[n]);
  
  for (n in 1:N) 
    y_pop_rep[n] = binomial_rng(K[n],  
                                 beta_rng(phi * kappa, 
                                          (1 - phi) * kappa)); 
 
  min_y_rep = min(y_rep); 
  max_y_rep = max(y_rep); 
  mean_y_rep = mean(to_vector(y_rep)); 
  sd_y_rep = sd(to_vector(y_rep)); 
 
  p_min = (min_y_rep >= min_y); 
  p_max = (max_y_rep >= max_y); 
  p_mean = (mean_y_rep >= mean_y); 
  p_sd = (sd_y_rep >= sd_y); 
}
