# Load necessary library
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(mc.cores = parallel::detectCores())

data_all <- read.csv("/Users/garyzhou/Downloads/stats.csv")[,-11]
data <- data.frame(data_all %>% filter(b_ab >= 100))

# Split the data into training and testing sets
train_data <- filter(data, year <= 2020)
test_data <- filter(data, year == 2021)

# Prepare the data
data_list <- list(
  N = nrow(train_data),
  b_home_run = train_data$b_home_run,
  player_age = train_data$player_age,
  b_ab = train_data$b_ab,
  exit_velocity_avg = train_data$exit_velocity_avg,
  launch_angle_avg = train_data$launch_angle_avg,
  N_new = nrow(test_data),
  player_age_new = test_data$player_age,
  b_ab_new = test_data$b_ab,
  exit_velocity_avg_new = test_data$exit_velocity_avg,
  launch_angle_avg_new = test_data$launch_angle_avg
)


# Compile the model
stan_model <- stan_model(file = "Bayesian_Linear_Regression.stan")

# Fit the model
fit <- sampling(stan_model, data = data_list, iter = 2000, chains = 4)


generated_quantities_fit <- rstan::extract(fit, pars = c("b_home_run_sim", "b_home_run_pred", "p_positive_effect_age", "p_positive_effect_ab", "p_positive_effect_velocity", "p_positive_effect_angle"))
ext_fit_new <- rstan::extract(fit)
mean(apply(ext_fit_new$b_home_run_pred, 2, median) == test_data$b_home_run)


squared_diff <- round((apply(ext_fit_new$b_home_run_pred, 2, median) - test_data$b_home_run)^2, 4)
squared_diff
mse_br_savant <- mean((apply(ext_fit_new$b_home_run_pred, 2, median) - test_data$b_home_run)^2)
rmse_br_savant <- sqrt(mse_br_savant)
rmse_br_savant

summary(test_data$b_home_run)
# Check the summary of the fit
#Plot of posterier estimates and credible intervals
plot(fit, pars=c("beta_age", "beta_ab", "beta_velocity", "beta_angle"))
#Pairwise correlation between the parameters
pairs(fit, pars=c("beta_age", "beta_ab", "beta_velocity", "beta_angle"))

#Trace Plot
traceplot(fit)
traceplot(fit, pars="alpha")

# Accuracy
ext_fit <- rstan::extract(fit)

# Extract posterior distributions
alpha_post <- ext_fit$alpha
beta_age_post <- ext_fit$beta_age
beta_ab_post <- ext_fit$beta_ab
beta_velocity_post <- ext_fit$beta_velocity
beta_angle_post <- ext_fit$beta_angle




rstanarm::posterior_predict(fit)
extract(fit)

# Make predictions for the test set
pred_data_list <- list(
  N = nrow(test_data),
  player_age = test_data$player_age,
  b_ab = test_data$b_ab,
  exit_velocity_avg = test_data$exit_velocity_avg,
  launch_angle_avg = test_data$launch_angle_avg
)
pred <- extract(fit, pars = "alpha, beta_age, beta_ab, beta_velocity, beta_angle, sigma") %>% 
  sapply(function(x) colMeans(x)) %>% 
  {.$alpha + .$beta_age * pred_data_list$player_age + .$beta_ab * pred_data_list$b_ab + .$beta_velocity * pred_data_list$exit_velocity_avg + .$beta_angle * pred_data_list$launch_angle_avg}

# Check the predictive performance
plot(test_data$b_home_run, pred, xlab = "True", ylab = "Predicted", main = "Predicted vs. True")
abline(a = 0, b = 1)
