# Load the necessary libraries
library(rstan)
library(caret)
library(tidyverse)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(mc.cores = parallel::detectCores())
# Read the data
data <- read.csv("/Users/garyzhou/Downloads/batter2015.csv")
str(data)
# Select the predictors and the outcome
X <- data[, c("player_age", "b_total_pa", "exit_velocity_avg", "launch_angle_avg")]
y <- data$b_home_run

# Define the data for Stan
stan_data <- list(N = nrow(X), K = ncol(X), X = as.matrix(X), y = y,
                  alpha = 1.0, rho = 0.1, sigma = 0.1)

# Compile the model
stan_model <- stan_model(file = "/Users/garyzhou/Downloads/Gaussian_Process_Stan.stan")

# Fit the model
stan_fit <- sampling(stan_model, data = stan_data, iter = 1000, chains = 2)

# Print the results
print(stan_fit)


summary(stan_fit)$summary




summary(data$launch_angle_avg)


#Attempt 6
#2022 Data
data_all <- read.csv("/Users/garyzhou/Downloads/stats (2).csv")[,-10]
data <- data.frame(data_all %>% filter(b_ab >= 100))
X <- data[, c("player_age", "b_ab", "exit_velocity_avg", "launch_angle_avg")]
y <- data$b_home_run

set.seed(42)  # Set a seed for reproducibility
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Define the data for Stan
stan_data <- list(
  N1 = nrow(X_train), 
  x1 = as.matrix(X_train),
  y1 = y_train,
  N2 = nrow(X_test), 
  x2 = as.matrix(X_test)
)

# Compile the model
stan_model <- stan_model(file = "/Users/garyzhou/Downloads/GP_Reg_Model6.stan")

# Fit the model
stan_fit <- sampling(stan_model, data = stan_data, iter = 1000, chains = 4)







#Attempt 5:
#2022 Data
data_all <- read.csv("/Users/garyzhou/Downloads/stats (2).csv")[,-10]
data <- data.frame(data_all %>% filter(b_ab >= 100))
str(data)
# Select the predictors and the outcome
#Original
#X <- data[, c("player_age", "b_total_pa", "exit_velocity_avg", "launch_angle_avg")]
#Test
X <- data[, c("player_age", "b_ab", "exit_velocity_avg", "launch_angle_avg")]
y <- data$b_home_run

set.seed(42)  # Set a seed for reproducibility
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Define the data for Stan
stan_data <- list(
  N_train = nrow(X_train), 
  N_test = nrow(X_test), 
  K = ncol(X_train), 
  X = as.matrix(X_train), 
  y = y_train,
  X_test = as.matrix(X_test),
  sigma = 0.1
)

# Compile the model
stan_model <- stan_model(file = "/Users/garyzhou/Downloads/GP_Reg_Model5.stan")

# Fit the model
stan_fit <- sampling(stan_model, data = stan_data, iter = 1000, chains = 4)


# Print the results
print(gp_regression_model)

#save(stan_fit, file = "/Users/garyzhou/Downloads/gp_regression_model_pp.rda")
gp_regression_model_pp <- stan_fit
pp_samples_gpr <- rstan::extract(gp_regression_model_pp)
summary(pp_samples_gpr$y_test)
str(pp_samples_gpr$y_test)
str(pp_samples_gpr$f_test)
print(gp_regression_model_pp)
str(pp_samples_gpr$alpha)
sstr(y_test)

# Compute posterior estimates of y_pred
y_pred <- apply(pp_samples_gpr$f_test, 2, mean)

# Calculate MSE and MAE
rmse_gpr <- sqrt(mean((y_test - y_pred)^2))
mae <- mean(abs(y_test - y_pred))

print(paste("MSE: ", mse))
print(paste("MAE: ", mae))


summary(gp_regression_model)$summary

#Predictive Accuracy
# Extract samples from the posterior distribution
posterior_samples_gpr <- rstan::extract(gp_regression_model)



# Compute posterior estimates of y_pred
y_pred <- apply(posterior_samples_gpr$f, 2, mean)

# Calculate MSE and MAE
rmse_gpr <- sqrt(mean((y_train - y_pred)^2))
mae <- mean(abs(y_test - y_pred))

print(paste("MSE: ", mse))
print(paste("MAE: ", mae))



#Attempt to get posterior predictive
# Extract posterior samples
alpha_samples <- posterior_samples_gpr$alpha
rho_samples <- posterior_samples_gpr$rho
sigma_samples <- posterior_samples_gpr$sigma
f_samples <- posterior_samples_gpr$f

# Calculate K_xt
# Initialize K_xt
K_xt <- array(0, dim = c(nrow(X_train), nrow(X_test), length(alpha_samples)))

# Calculate K_xt for each pair of training and test data points, for each sample
for (s in 1:length(alpha_samples)) {
  for (i in 1:nrow(X_test)) {
    for (j in 1:nrow(X_train)) {
      K_xt[j, i, s] <- alpha_samples[s] * exp(-rho_samples[s] * sum((X_train[j, ] - X_test[i, ])^2))
    }
  }
}

# Now compute y_pred for each sample
y_pred_samples <- matrix(0, nrow = length(alpha_samples), ncol = nrow(X_test))
for (s in 1:length(alpha_samples)) {
  for (i in 1:nrow(X_test)) {
    y_pred_samples[s, i] <- rnorm(1, sum(K_xt[, i, s] * f_samples[s, ]) / sigma_samples[s], 
                                  sqrt(alpha_samples[s] + sigma_samples[s] - 
                                         sum(K_xt[, i, s] * f_samples[s, ]) / sigma_samples[s]))
  }
}

# Now calculate the mean y_pred across all samples
y_pred <- colMeans(y_pred_samples)


















#Attempt 2: It works
data <- read.csv("/Users/garyzhou/Downloads/batter2015.csv")
data_all <- read.csv("/Users/garyzhou/Downloads/stats.csv")[,-11]
data <- data.frame(data_all %>% filter(b_ab >= 100))
str(data)
# Select the predictors and the outcome
#Original
#X <- data[, c("player_age", "b_total_pa", "exit_velocity_avg", "launch_angle_avg")]
#Test
X <- data[, c("player_age", "b_ab", "exit_velocity_avg", "launch_angle_avg")]
y <- data$b_home_run

# Define the data for Stan
stan_data <- list(N = nrow(X), K = ncol(X), X = as.matrix(X), y = y,
                  sigma = 0.1)

# Compile the model
stan_model <- stan_model(file = "/Users/garyzhou/Downloads/Gaussian_Process.stan")

# Fit the model
stan_fit <- sampling(stan_model, data = stan_data, iter = 1000, chains = 2)

# Print the results
print(stan_fit)

save(stan_fit, file = "/Users/garyzhou/Downloads/stan_fit_test.rda")

summary(stan_fit)$summary


# Load the coda library
library(coda)

stan_trace(stan_fit, pars=c("alpha", "rho"))
stan_ac(stan_fit, pars=c("alpha", "rho"))
stan_ac(stan_fit, pars=c("alpha", "rho"))

### Test ###
#AC values
alpha_samples <- extract(stan_fit, "alpha")[[1]]

# Compute the autocorrelation function
alpha_acf <- acf(alpha_samples, plot = FALSE)

# Extract the autocorrelation at lag 1
alpha_lag1_autocorrelation <- alpha_acf$acf[2]

# Print the autocorrelation at lag 1
print(alpha_lag1_autocorrelation)




coda::gelman.plot(As.mcmc.list(stan_fit, pars =c("alpha", "rho")), autoburnin = F)
# Calculate the Gelman-Rubin diagnostic #rhat
print(stan_rhat(stan_fit))

gelman.diag(mcmc_chain)

# Calculate the Effective Sample Size
effectiveSize(mcmc_chain)






#Attempt 3: 
data <- read.csv("/Users/garyzhou/Downloads/batter2015.csv")

train_data <- data[data$year < 2021,]
test_data <- data[data$year == 2021,]


# Select the predictors and the outcome
X_old <- data[, c("player_age", "b_ab", "exit_velocity_avg", "launch_angle_avg")]
X <- scale(X_old)
y <- data$b_home_run



# Define the data for Stan
stan_data <- list(N = nrow(X), K = ncol(X), X = as.matrix(X), y = y)

# Compile the model
stan_model <- stan_model(file = "/Users/garyzhou/Downloads/GP_Reg_Model3.stan")

# Fit the model
stan_fit <- sampling(stan_model, data = stan_data, iter = 2000, chains = 4)





#Attempt 4: 
data_all <- read.csv("/Users/garyzhou/Downloads/stats.csv")[,-11]
data <- data.frame(data_all %>% filter(b_ab >= 100))

train_data <- data[data$year < 2021,]
test_data <- data[data$year == 2021,]

# Select the predictors and the outcome
X_old_train <- train_data[, c("player_age", "b_ab", "exit_velocity_avg", "launch_angle_avg")]
X_train <- scale(X_old_train)
X_old_test <- test_data[, c("player_age", "b_ab", "exit_velocity_avg", "launch_angle_avg")]
X_test <- scale(X_old_test)
y_train <- train_data$b_home_run
y_test <- test_data$b_home_run

# Define the data for Stan
#stan_data <- list(N = nrow(X), K = ncol(X), X = as.matrix(X), y = y)
stan_data <- list(
  N = nrow(X_train),
  K = ncol(X_train),
  X = as.matrix(X_train),
  y = y_train,
  N_new = nrow(X_test),
  X_new = as.matrix(X_test)
)

# Compile the model
compiled_modell <- stan_model(file = "/Users/garyzhou/Downloads/GP_Reg_Model4.stan")

# Fit the model
stan_fit <- sampling(compiled_modell, data = stan_data, iter = 2000, chains = 4, seed=123)










savant_data <- read.csv("/Users/garyzhou/Downloads/bballsavant.csv")
data <- savant_data %>% filter(year==2021)
str(data)
# Compute home run rate
data$hr_rate <- data$b_home_run / data$b_ab

# Split the data into a training and test set
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(data$hr_rate, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]



# Fit the model
fit <- stan(
  file = "gp_regression_savant_hr.stan", 
  data = list(
    N = nrow(dataTrain),
    age = dataTrain$player_age,
    velocity = dataTrain$exit_velocity_avg,
    angle = dataTrain$launch_angle_avg,
    atbat = dataTrain$b_ab,
    hr_rate = dataTrain$b_home_run,
    N_new = nrow(dataTest),
    age_new = dataTest$player_age,
    velocity_new = dataTest$exit_velocity_avg,
    angle_new = dataTest$launch_angle_avg,
    atbat_new = dataTest$b_ab
  ),
  chains = 4,
  iter = 2000,
)

# Extract the posterior samples
posterior_samples <- rstan::extract(fit)
head(posterior_samples)
# Posterior predictive checks
pp_check(fit)

# Extract the predictions for the test set
hr_rate_new <- posterior_samples$hr_rate_new

# Compute the mean and 95% CI of the predictions
hr_rate_new_mean <- apply(hr_rate_new, 2, mean)
hr_rate_new_lci <- apply(hr_rate_new, 2, quantile, probs = 0.025)
hr_rate_new_uci <- apply(hr_rate_new, 2, quantile, probs = 0.975)

# Combine the mean and CI into a data frame
predictions <- data.frame(
  mean = hr_rate_new_mean,
  lci = hr_rate_new_lci,
  uci = hr_rate_new_uci
)

# View the predictions
print(predictions)

















# Prepare data for Stan model
stan_data <- list(
  N = nrow(dataTrain),
  year = as.integer(dataTrain$year),
  age = dataTrain$player_age,
  velocity = dataTrain$exit_velocity_avg,
  angle = dataTrain$launch_angle_avg,
  hr_rate = dataTrain$hr_rate
)

# Compile and fit Stan model
fit <- stan(file = 'gp_regression_savant.stan', data = stan_data)


