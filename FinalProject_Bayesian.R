install.packages("Lahman")
install.packages("rstan")
install.packages("rstanarm")




##Test 1 Stan Two Component Mixture

# Load the necessary library
library(Lahman)
library(rstan)
library(rstanarm)
library(tidyverse)
library(bayesplot)
library(ggplot2)

library(Lahman)
data("Batting")

# Subset the Batting data for seasons after 1975
season_all_relevant <- subset(Batting, yearID > 1975)

allseason_min_atbat <- season_all_relevant %>% filter(AB >= 100)
allseason_min_ab <- allseason_min_atbat %>% filter(HR>0)
hr <- allseason_min_ab$HR
ab <- allseason_min_ab$AB
hrprop <- hr/ab
Y <- hrprop
n <- length(Y)
year <- allseason_min_ab$yearID
player <- allseason_min_ab$playerID


#Failed Attempt
# Read the data
hitters <- read.csv("stats.csv")
hr <- hitters$b_home_run
ab <- hitters$b_ab
hrprop <- hr/ab
Y <- hrprop
n <- length(Y)

# Define the Stan model
stan_code <- "
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
"

#Attempt Two##
# Define the Stan model
stan_code <- "
data {
    int<lower=0> n; // Number of data points
    vector[n] Y; // Data
}
parameters {
    real<lower=0, upper=1> alpha; // Mixing proportion
    ordered[2] mu; // Means of the two components
    real<lower=0> sigma[2]; // Standard deviations of the two components
}
model {
    alpha ~ beta(1, 1);
    mu[1] ~ normal(0.06, 1);
    mu[2] ~ normal(0.01, 1);
    sigma ~ inv_gamma(2, 0.1);
    for (i in 1:n) {
        target += log_mix(alpha,
                          normal_lpdf(Y[i] | mu[1], sigma[1]),
                          normal_lpdf(Y[i] | mu[2], sigma[2]));
    }
}
"

#Try generated quantities
stan_code <- "
data {
    int<lower=0> n; // Number of data points
    vector[n] Y; // Data
}
parameters {
    real<lower=0, upper=1> alpha; // Mixing proportion
    ordered[2] mu; // Means of the two components
    real<lower=0> sigma[2]; // Standard deviations of the two components
}
model {
    alpha ~ beta(1, 1);
    mu[1] ~ normal(0.06, 1);
    mu[2] ~ normal(0.01, 1);
    sigma ~ inv_gamma(2, 0.1);
    for (i in 1:n) {
        target += log_mix(alpha,
                          normal_lpdf(Y[i] | mu[1], sigma[1]),
                          normal_lpdf(Y[i] | mu[2], sigma[2]));
    }
}
generated quantities {
  vector[n] Y_hat;
  for (i in 1:n) {
    Y_hat[i] = normal_rng(mu[1], sigma[1]) * alpha + normal_rng(mu[2], sigma[2]) * (1 - alpha);
  }
}
"

#Last Try for Posterior Predictive
stan_code <- "
data {
    int<lower=0> n; // Number of data points
    vector[n] Y; // Data
}
parameters {
    real<lower=0, upper=1> alpha; // Mixing proportion
    real<lower=0, lower_bound=mu[1]> mu[2]; // Means of the two components
    real<lower=0> sigma[2]; // Standard deviations of the two components
}
model {
    alpha ~ beta(1, 1);
    mu[1] ~ normal(0.06, 1);
    mu[2] ~ normal(0.01, 1);
    sigma ~ inv_gamma(2, 0.1);
    for (i in 1:n) {
        target += log_mix(alpha,
                          normal_lpdf(Y[i] | mu[1], sigma[1]),
                          normal_lpdf(Y[i] | mu[2], sigma[2]));
    }
}
generated quantities {
  vector[n] Y_hat;
  for (i in 1:n) {
    Y_hat[i] = normal_rng(mu[1], sigma[1]) * alpha + normal_rng(mu[2], sigma[2]) * (1 - alpha);
  }
}
"

#Last Last Try
stan_code <- "
data {
    int<lower=0> n; // Number of data points
    vector[n] Y; // Data
}
parameters {
    real<lower=0, upper=1> alpha; // Mixing proportion
    real<lower=0> mu1; // Mean of the first component
    real<lower=0> delta_mu; // Difference between means of the components
    real<lower=0> sigma[2]; // Standard deviations of the two components
}
transformed parameters {
    real<lower=0> mu2; // Mean of the second component
    mu2 = mu1 + delta_mu;
}
model {
    alpha ~ beta(1, 1);
    mu1 ~ normal(0, 0.2);
    delta_mu ~ normal(0, 0.2);
    sigma ~ inv_gamma(2, 0.1);
    for (i in 1:n) {
        target += log_mix(alpha,
                          normal_lpdf(Y[i] | mu1, sigma[1]),
                          normal_lpdf(Y[i] | mu2, sigma[2]));
    }
}
"

#Last Last Try (Incorporate Pred Post)
stan_code <- "
data {
    int<lower=0> n; // Number of data points
    vector[n] Y; // Data
}
parameters {
    real<lower=0, upper=1> alpha; // Mixing proportion
    real<lower=0> mu1; // Mean of the first component
    real<lower=0> delta_mu; // Difference between means of the components
    real<lower=0> sigma[2]; // Standard deviations of the two components
}
transformed parameters {
    real<lower=0> mu2; // Mean of the second component
    mu2 = mu1 + delta_mu;
}
model {
    alpha ~ beta(1, 1);
    mu1 ~ normal(0, 0.2);
    delta_mu ~ normal(0, 0.2);
    sigma ~ inv_gamma(2, 0.1);
    for (i in 1:n) {
        target += log_mix(alpha,
                          normal_lpdf(Y[i] | mu2, sigma[2]),
                          normal_lpdf(Y[i] | mu1, sigma[1]));
    }
}
generated quantities {
    vector[n] Y_pred;
    for (i in 1:n) {
        real theta = bernoulli_rng(alpha);
        Y_pred[i] = normal_rng((1 - theta) * mu1 + theta * mu2, (1 - theta) * sigma[1] + theta * sigma[2]);
    }
}
"

options(mc.cores = parallel::detectCores())
rstan::rstan_options(mc.cores = parallel::detectCores())
# Compile the model
stan_model <- stan_model(model_code = stan_code)

# Fit the model to the data
fit <- sampling(stan_model, data = list(n = n, Y = Y), iter = 3000, chains = 4)

setwd("/Users/garyzhou/Downloads")
#save(fit, file = "trunc_norm_mixture_mod.rda")
#save(fit, file = "mixture_mod_fit_pp.rda")
#save(fit, file = "mixture_mod_fit.rda")

# Print the fit
print(fit)

# Posterior predictive checks
pp_check(fit)

# Trace plots
mcmc_trace(fit, pars = c("alpha", "mu[1]", "mu[2]", "sigma[1]", "sigma[2]"))

# Autocorrelation plots
mcmc_acf_bar(fit, pars = c("alpha", "mu[1]", "mu[2]", "sigma[1]", "sigma[2]"))
mcmc_acf(fit, pars = c("alpha", "mu[1]", "mu[2]", "sigma[1]", "sigma[2]"))

# Generate posterior predictive samples
#PP Check Test
posterior_samples_test <- as.matrix(fit)
simulated_data_test <- stan_model$call$sample_fn(posterior_samples_test)

#Ex
n_sims <- length(schools_sim$lp__)
y_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims)
  y_rep[s,] <- rnorm(J, schools_sim$theta[s,], sigma)
#########
rstan::posterior_predict(fit)

post_samples_mix <- rstan::extract(fit)
print(fit)

# Extract the posterior samples for mixture (non-trunc)
alpha_samp <- post_samples_mix$alpha
mu1_samp <- post_samples_mix$mu[,1]
mu2_samp <- post_samples_mix$mu[,2]
sigma1_samp <- post_samples_mix$sigma[,1]
sigma2_samp <- post_samples_mix$sigma[,2]

# Extract the posterior samples for mixture mod truncated
alpha_samp <- post_samples_mix$alpha
mu1_samp <- post_samples_mix$mu1
mu2_samp <- post_samples_mix$mu2
sigma1_samp <- post_samples_mix$sigma[,1]
sigma2_samp <- post_samples_mix$sigma[,2]

#Quantiles for Table
percentiles_alpha <- quantile(alpha_samp, probs = c(0.1, 0.9))
percentiles_alpha
mean(alpha_samp)
percentiles_mu1 <- quantile(mu1_samp, probs = c(0.1, 0.9))
percentiles_mu1
mean(mu1_samp)
percentiles_mu2 <- quantile(mu2_samp, probs = c(0.1, 0.9))
percentiles_mu2
mean(mu2_samp)
percentiles_sigma1 <- quantile(sigma1_samp, probs = c(0.1, 0.9))
percentiles_sigma1
mean(sigma1_samp)
percentiles_sigma2 <- quantile(sigma2_samp, probs = c(0.1, 0.9))
percentiles_sigma2
mean(sigma2_samp)

alpha <- post_samples_mix$alpha
mu1 <- post_samples_mix$mu1
mu2 <- post_samples_mix$mu2
sigma1 <- post_samples_mix$sigma[,1]
sigma2 <- post_samples_mix$sigma[,2]


# Extract the posterior predictive samples
Y_hat_samples <- rstan::extract(fit, "Y_hat") #4000 rows of 17686 columns
# Plot histograms of the observed data and the generated data
hist(Y, main = "Observed Data")

Y_hat_test <- colMeans(post_pred_samp)
hist(Y_hat_test, main = "Generated Data")




alpha<- post_samples_mix$alpha
mu1 <- post_samples_mix$mu[,1]
mu2 <- post_samples_mix$mu[,2]
sigma1 <- post_samples_mix$sigma[,1]
sigma2 <- post_samples_mix$sigma[,2]

mean(mu2)
# Generate the sequence for x-axis
x_seq <- seq(min(Y), max(Y), length.out = 4000)

# Calculate the densities
#pp_testt <- sapply(1:length(alpha_samp), function(i) alpha_samp[i] * dnorm(x_seq, mu1_samp[i], sigma1_samp[i]) + (1 - alpha_samp[i]) * dnorm(x_seq, mu2_samp[i], sigma2_samp[i]))
#length(alpha)
density_mix <- rowMeans(sapply(1:length(alpha_samp), function(i) alpha_samp[i] * dnorm(x_seq, mu1_samp[i], sigma1_samp[i]) + (1 - alpha_samp[i]) * dnorm(x_seq, mu2_samp[i], sigma2_samp[i])))
density1 <- rowMeans(sapply(1:length(mu1_samp), function(i) alpha_samp[i] * dnorm(x_seq, mu1_samp[i], sigma1_samp[i])))
density2 <- rowMeans(sapply(1:length(mu2_samp), function(i) (1 - alpha_samp[i]) * dnorm(x_seq, mu2_samp[i], sigma2_samp[i])))

# Plot the histogram and the densities
hist(Y, freq = FALSE, main = "Home Run Mixture Model", xlab = "Home Run Proportion", ylab = "Density", ylim = c(0, 30))
lines(x_seq, density_mix, col = "blue")
lines(x_seq, density1, col = "red")
lines(x_seq, density2, col = "green")
legend("topright", legend = c("Mixture", "Non-Elite", "Elite"), col = c("blue", "red", "green"), lty = 1)        





#Y_hat <- sapply(1:length(mu1), function(i) rnorm(length(Y), mu1[i], sigma1[i]) * alpha[i] + rnorm(length(Y), mu2[i], sigma2[i]) * (1 - alpha[i]))


max_yhat <- apply(Y_hat, 1, max)
hist(max_yhat)

max(Y)

min_yhat <- apply(Y_hat, 1, min)
hist(min_yhat)
min(Y)

mean_yhat <- apply(Y_hat, 1, mean)

#Posterior Predictive Check
test_n <- length(Y) # Or whatever the length of your future data should be

Y_pred <- matrix(NA, nrow = test_n, ncol = length(alpha_samp))
Y_pred <- matrix(NA, nrow = test_n, ncol = 4000)

for (i in 1:4000) {
  alpha <- alpha_samp[i]
  mu1 <- mu1_samp[i]
  mu2 <- mu2_samp[i]
  sigma1 <- sigma1_samp[i]
  sigma2 <- sigma2_samp[i]
  
  theta <- rbinom(test_n, size = 1, prob = alpha)
  Y_pred[, i] <- rnorm(test_n, mean = (theta) * mu1 + (1-theta) * mu2, sd = (theta) * sigma1 + (1-theta) * sigma2)
}

for (i in 1:4000) {
  alpha <- alpha_samp[i]
  mu1 <- mu1_samp[i]
  mu2 <- mu2_samp[i]
  sigma1 <- sigma1_samp[i]
  sigma2 <- sigma2_samp[i]
  
  theta <- rbinom(test_n, size = 1, prob = alpha)
  Y_pred[, i] <- rnorm(test_n, mean = (1 - theta) * mu1 + theta * mu2, sd = (1 - theta) * sigma1 + theta * sigma2)
}

head(Y_pred)[,1:5]
hist(Y_pred)
summary_observed <- c(min = min(Y),
                      max = max(Y),
                      mean = mean(Y),
                      sd = sd(Y))

# Calculate summary statistics for each set of predicted data
summary_predicted <- apply(Y_pred, 2, function(y) {
  c(min = min(y, na.rm=TRUE),
    max = max(y, na.rm=TRUE),
    mean = mean(y, na.rm=TRUE),
    sd = sd(y, na.rm=TRUE))
})


pp_min <- summary_predicted[1,]
pp_max <- summary_predicted[2,]
pp_mean <- summary_predicted[3,]
pp_sd <- summary_predicted[4,]

ggplot(data.frame(pp_mean), aes(x = pp_mean)) +
  geom_histogram() +
  geom_vline(xintercept = mean(Y), color = "red")+
  labs(x = "Mean HR Rate")

ggplot(data.frame(pp_min), aes(x = pp_min)) +
  geom_histogram() +
  geom_vline(xintercept = min(Y), color = "red")+
  labs(x = "Minimum HR Rate")
ggplot(data.frame(pp_max), aes(x = pp_max)) +
  geom_histogram() +
  geom_vline(xintercept = max(Y), color = "red")+
  labs(x = "Max HR Rate")
ggplot(data.frame(pp_sd), aes(x = pp_sd)) +
  geom_histogram() +
  geom_vline(xintercept = sd(Y), color = "red")+
  labs(x = "SD HR Rate")
hist(summary_predicted[2,])
hist(summary_predicted[3,])
hist(summary_predicted[4,])

head(apply(Y_pred, 2, min))

#Ranking
print(fit)
hitters <- allseason_min_ab
library(xtable)
ortiz<-hitters[hitters[,1]=="ortizda01",] 
ortiz.HRavg<-ortiz$HR/ortiz$AB
prob.elite.ortiz <- Estep(ortiz.HRavg, alpha, mu1, mu2, sigsq) 
prob.elite.ortiz <- cbind(hitters[hitters[,1]=="ortizda01",2],
                          prob.elite.ortiz)
xtable(prob.elite.ortiz , digits = c(0,0,6))

#Getting Individual probabilities for each player
Estep <- function(y, alpha, mu1, mu2, sigma1, sigma2){
  n <- length(y)  
  ind <- rep(NA,n)
  for (i in 1:n){
    prob1 <- (1-alpha)*dnorm(y[i], mean=mu1, sd=sigma1)
    prob2 <- alpha*dnorm(y[i], mean=mu2, sd=sigma2)
    ind[i] <- prob2/(prob1+prob2)
  }
  ind
}

Estep <- function(y, alpha, mu1, mu2, sigma1, sigma2){
  n <- length(y)  
  ind <- rep(NA,n)
  for (i in 1:n){
    prob1 <- (alpha)*dnorm(y[i], mean=mu1, sd=sigma1)
    prob2 <- (1-alpha)*dnorm(y[i], mean=mu2, sd=sigma2)
    ind[i] <- prob2/(prob1+prob2)
  }
  ind
}

Estep_nopower <- function(y, alpha, mu1, mu2, sigma1, sigma2){
  n <- length(y)  
  ind <- rep(NA,n)
  for (i in 1:n){
    prob1 <- (alpha)*dnorm(y[i], mean=mu1, sd=sigma1)
    prob2 <- (1-alpha)*dnorm(y[i], mean=mu2, sd=sigma2)
    ind[i] <- prob1/(prob1+prob2)
  }
  ind
}

post_pred_mean <- apply(Y_pred, 1, mean)
min(post_pred_mean)

hist(post_pred_mean)

finalindprops <- Estep_nopower(hrprop, 
                       mean(alpha_samp), 
                       mean(mu1_samp), 
                       mean(mu2_samp), 
                       mean(sigma1_samp), 
                       mean(sigma2_samp))

finalindprops <- Estep(hrprop, 
                               mean(alpha_samp), 
                               mean(mu1_samp), 
                               mean(mu2_samp), 
                               mean(sigma1_samp), 
                               mean(sigma2_samp))


hist(finalindprops)
sum(finalindprops > 0.5)
sum(finalindprops > 0.5)/length(finalindprops)
sum(finalindprops > 0.9999)
players.topHR<-allseason_min_ab[finalindprops > 0.9999,]
top_hr_hitter_2021 <- players.topHR %>% filter(yearID == 2021)

top_sluggers_2021 <- top_hr_hitter_2021$playerID

ortiz<-hitters[hitters[,1]=="ortizda01",] 
ortiz.HRavg<-ortiz$HR/ortiz$AB
prob.elite.ortiz <- Estep(ortiz.HRavg, alpha, mu1, mu2, sigsq) 
prob.elite.ortiz <- cbind(hitters[hitters[,1]=="ortizda01",2],
                          prob.elite.ortiz)


top_sluggers_2021
sort(finalindprops, decreasing =TRUE)

##########
hitters <- read.csv("stats.csv")
head(hitters)


hr <- hitters$b_home_run
ab <- hitters$b_ab
year <- hitters$year
player <- hitters$player_id

#Calculating homerun proportion:
hrprop <- hr/ab
hist(hrprop)
Y <- hrprop
n <- length(Y)








#### Test 2023 ####
library(Lahman)
data("Batting")

# Subset the Batting data for seasons after 1975
season_all_relevant <- subset(Batting, yearID > 1975)

allseason_min_atbat <- season_all_relevant %>% filter(AB >= 100)
allseason_min_ab <- allseason_min_atbat %>% filter(HR>0)
hr <- allseason_min_ab$HR
ab <- allseason_min_ab$AB
hrprop <- hr/ab
Y <- hrprop
n <- length(Y)
year <- allseason_min_ab$yearID
player <- allseason_min_ab$playerID

# starting point
alpha <- 0.1
mu0 <- 0.02
mu1 <- 0.06
sigsq0 <- 0.005
sigsq1 <- 0.01

# gibbs sampler
numiters <- 2000
params <- matrix(NA, nrow=numiters, ncol=5)
params[1,] <- c(alpha, mu0, mu1, sigsq0, sigsq1)
inds <- matrix(NA, nrow=numiters-1, ncol=n)
for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA,n)
  for (j in 1:n){
    a <- dnorm(Y[j], mu1, sqrt(sigsq1))*alpha    
    b <- dnorm(Y[j], mu0, sqrt(sigsq0))*(1-alpha)
    p <- a/(a+b)
    I[j] <- rbinom(1, 1, p)
  }
  ## calculating statistics from indicator variables
  n0 <- sum(I==0)
  n1 <- sum(I==1)
  meanY0 <- mean(Y[I==0])
  meanY1 <- mean(Y[I==1])
  ## sampling alphas
  alpha <- rbeta(1,n1+1,n0+1)
  ## sampling means
  mu0 <- rnorm(1,meanY0,sqrt(sigsq0/n0))
  mu1 <- rnorm(1,meanY1,sqrt(sigsq1/n1))
  ## calculating statistics from new means 
  ss0 <- sum((Y[I==0]-mu0)^2)
  ss1 <- sum((Y[I==1]-mu1)^2)
  ## sampling variances
  temp <- rgamma(1, shape=(n0/2)+1, rate=ss0/2)
  sigsq0 <- 1/temp
  temp <- rgamma(1, shape=(n1/2)+1, rate=ss1/2)
  sigsq1 <- 1/temp
  ## storing current values
  params[i,] <- c(alpha, mu0, mu1, sigsq0, sigsq1)
  inds[i-1,] <- I
  print(i)
  #print(round(c(alpha,mu0,mu1,sigsq0,sigsq1),5))
}

params.data1 <- params 
params.data1
#running Gibbs sampler for alternate starting point with code above

alpha <- 0.80
mu0 <- 0.03
mu1 <- 0.05
sigsq0 <- 0.005
sigsq1 <- 0.003

for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA,n)
  for (j in 1:n){
    a <- dnorm(Y[j], mu1, sqrt(sigsq1))*alpha    
    b <- dnorm(Y[j], mu0, sqrt(sigsq0))*(1-alpha)
    p <- a/(a+b)
    I[j] <- rbinom(1, 1, p)
  }
  ## calculating statistics from indicator variables
  n0 <- sum(I==0)
  n1 <- sum(I==1)
  meanY0 <- mean(Y[I==0])
  meanY1 <- mean(Y[I==1])
  ## sampling alphas
  alpha <- rbeta(1,n1+1,n0+1)
  ## sampling means
  mu0 <- rnorm(1,meanY0,sqrt(sigsq0/n0))
  mu1 <- rnorm(1,meanY1,sqrt(sigsq1/n1))
  ## calculating statistics from new means 
  ss0 <- sum((Y[I==0]-mu0)^2)
  ss1 <- sum((Y[I==1]-mu1)^2)
  ## sampling variances
  temp <- rgamma(1, shape=(n0/2)+1, rate=ss0/2)
  sigsq0 <- 1/temp
  temp <- rgamma(1, shape=(n1/2)+1, rate=ss1/2)
  sigsq1 <- 1/temp
  ## storing current values
  params[i,] <- c(alpha, mu0, mu1, sigsq0, sigsq1)
  inds[i-1,] <- I
  print(i)
  #print(round(c(alpha,mu0,mu1,sigsq0,sigsq1),5))
}

params.data2 <- params 
params.data2
# comparing iterations between two chains 
par(mfrow=c(1,1))
plot(1:numiters, params.data2[,1], main="alpha", type="l", col=3, xlab="Number of Iterations", ylab="alpha")
lines(1:numiters,params.data1[,1],col=2)

par(mfrow=c(3,2))
ymin <- min(params.data2[,1],params.data1[,1])
ymax <- max(params.data2[,1],params.data1[,1])
plot(1:numiters,params.data2[,1],main="alpha",type="l",col=3,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="alpha")
lines(1:numiters,params.data1[,1],col=2)

ymin <- min(params.data2[,2],params.data1[,2])
ymax <- max(params.data2[,2],params.data1[,2])
plot(1:numiters,params.data2[,2],main="mu1",type="l",col="green",ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu1")
lines(1:numiters,params.data1[,2],col=2)

ymin <- min(params.data2[,3],params.data1[,3])
ymax <- max(params.data2[,3],params.data1[,3])
plot(1:numiters,params.data2[,3],main="mu2",type="l",col="green",ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu2")
lines(1:numiters,params.data1[,3],col=2)

ymin <- min(params.data2[,4],params.data1[,4])
ymax <- max(params.data2[,4],params.data1[,4])
plot(1:numiters,params.data2[,4],main="sigsq1",type="l",col="green",ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq1")
lines(1:numiters,params.data1[,4],col=2)

ymin <- min(params.data2[,5],params.data1[,5])
ymax <- max(params.data2[,5],params.data1[,5])
plot(1:numiters,params.data2[,5],main="sigsq2",type="l",col="green",ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq2")
lines(1:numiters,params.data1[,5],col=2)

params.data1.postburn<-params.data1[201:2000,]
params.data2.postburn<-params.data2[201:2000,]

par(mfrow=c(3,2))
acf(params.data1.postburn[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.postburn[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.postburn[,2],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.postburn[,2],lag.max=100,main="ACF: mu1, chain 2")
acf(params.data1.postburn[,3],lag.max=100,main="ACF: mu2, chain 1")
acf(params.data2.postburn[,3],lag.max=100,main="ACF: mu2, chain 2")

par(mfrow=c(2,2))
acf(params.data1.postburn[,4],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.postburn[,4],lag.max=100,main="ACF: sigsq1, chain 2")
acf(params.data1.postburn[,5],lag.max=100,main="ACF: sigsq2, chain 1")
acf(params.data2.postburn[,5],lag.max=100,main="ACF: sigsq2, chain 2")

# taking only every fiftieth draw 
temp <- 50*c(1:(1800/50))

params.data1.thinned <- params.data1.postburn[temp,]
params.data2.thinned <- params.data2.postburn[temp,]

par(mfrow=c(3,2))
acf(params.data1.thinned[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.thinned[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.thinned[,2],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.thinned[,2],lag.max=100,main="ACF: mu1, chain 2")
acf(params.data1.thinned[,3],lag.max=100,main="ACF: mu2, chain 1")
acf(params.data2.thinned[,3],lag.max=100,main="ACF: mu2, chain 2")

par(mfrow=c(2,2))
acf(params.data1.thinned[,4],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.thinned[,4],lag.max=100,main="ACF: sigsq1, chain 2")
acf(params.data1.thinned[,5],lag.max=100,main="ACF: sigsq2, chain 1")
acf(params.data2.thinned[,5],lag.max=100,main="ACF: sigsq2, chain 2")

# combining chains and calculating posterior intervals
params.final <- rbind(params.data1.thinned,params.data2.thinned)

numsamples <- length(params.final[,1])
alpha.gibbs <- params.final[,1]
mu0.gibbs <- params.final[,2]
mu1.gibbs <- params.final[,3]
sigsq0.gibbs <- params.final[,4]
sigsq1.gibbs <- params.final[,5]

par(mfrow=c(3,2))
hist(mu0.gibbs, main="mu1")
hist(mu1.gibbs, main="mu2")
hist(sigsq0.gibbs, main="sigsq1")
hist(sigsq1.gibbs, main="sigsq2")
hist(alpha.gibbs, main="alpha")

summary(mu0.gibbs)
summary(mu1.gibbs)
summary(sigsq0.gibbs)
summary(sigsq1.gibbs)
summary(alpha.gibbs)

# fitted densities
par(mfrow=c(1,1))
hist(hrprop,main="Home Run Proportions",xlab="Home Run Proportions",col="gray")
numY <- length(Y)
xpoints <-ppoints(1000)*0.15
for (i in 1:numsamples){
  ylines <- 100*(alpha.gibbs[i]*dnorm(xpoints,mu1.gibbs[i],sqrt(sigsq1.gibbs[i])) + (1-alpha.gibbs[i])*dnorm(xpoints,mu0.gibbs[i],sqrt(sigsq0.gibbs[i])))
  lines(xpoints,ylines,col="purple")
  y1 <- 100*(alpha.gibbs[i])*dnorm(xpoints, mu1.gibbs[i], sqrt(sigsq1.gibbs[i]))
  lines(xpoints,y1,col="blue")
  y2 <- 100*((1-alpha.gibbs[i])*dnorm(xpoints, mu0.gibbs[i], sqrt(sigsq0.gibbs[i])))
  lines(xpoints,y2,col="red")
}

hist(hrprop,main="Home Run Proportions",xlab="Home Run Proportions",col="gray")
y1 <- (alpha)*dnorm(xpoints, mu1.gibbs, sqrt(sigsq1.gibbs))
lines(xpoints, y1, col="green")

#############################################
# plotting equal-variances fitted mixture density
alpha <- finalparam [1]
mu1 <-
  mu2 <-
  sigsq <- finalparam [4] par(mfrow=c(1,1))
finalparam [2] finalparam [3]
10
hist(hrprop,prob=T, ylim = c(0,25))
x <- ppoints(1000)*0.15
y1 <- (1-alpha)*dnorm(x,mu1,sqrt(sigsq))
y2 <- alpha*dnorm(x,mu2,sqrt(sigsq))
y<- (1-alpha)*dnorm(x,mu1,sqrt(sigsq)) + alpha*dnorm(x,mu2,sqrt(sigsq)) 
lines(x,y1,col='green')
lines(x,y2,col='red')
lines(x,y,col='purple', lwd=2,lty=2)
legend('topright', legend=c('Non-elite group', 'Elite group', 'Mixture'),
        col=c('green','red', 'purple'), lty=c(1,1,2), lwd=c(1,1,2))
#############################################



#Individual HR Probabilities for Players
prob.elite.hrprop <- Estep(hrprop, alpha, mu1, mu2, sigsq)
#Find out the playerIDs of player seasons that have a greater than 50% chance eliteplayernames <- playerID[which(prob.elite.hrprop > 0.50)]
#Find out the years of player seasons that have a greater than 50% chance of eliteplayeryears <- yearID[which(prob.elite.hrprop > 0.50)]
#Find out the home run rates and totals of player seasons that have a greater eliteplayerhrprop <- hrprop[which(prob.elite.hrprop > 0.50)]
eliteplayerhrtot <- HR[which(mu1.gibbs > 0.50)]
hr[which(mu1.gibbs > 0.50)]
length(mu1.gibbs)


feliteplayerhrtot <- HR[which(mu > 0.50)]
#Get the probability of being in the elite group for player seasons we've ide prob.elite <- Estep(eliteplayerhrprop, alpha, mu1, mu2, sigsq)
#Combine columns and put into table
elite.hrhitters <- cbind(eliteplayernames, eliteplayeryears, prob.elite, elit final_table <- xtable(elite.hrhitters)
                         names(final_table) <- c("Player Names", "Year", "Elite Group Probability", "H #Filter out those players who have had seasons of elite home run hitting final_table %>% arrange(desc(prob.elite)) %>% count()
final_table %>% arrange(desc(prob.elite)) %>% head(5)
final_table %>% arrange(prob.elite) %>% head(5)


































#############################################################################
stanmod <- stan_model("first_stan_model.stan")
draws_stan <- sampling(stanmod,
                       data = list(N = nrow(mtcars),
                                   x = mtcars$wt,
                                   y = mtcars$mpg),
                       seed = 47,
                       algorithm = "HMC",
                       chains = 1)
draws_stan
plot(draws_stan)
traceplot(draws_stan)
#########
library(rstan)
library(gamlss)
library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
#install.packages("Lahman")
#Remove Pitchers
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>% group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB) #%>% ungroup()


#First and Last Name



#Number of Appearances at Positions
#Find primary position for each player season. use as regression covar
head(Appearances)
nrow(Appearances %>% filter(yearID >= 2005)) #24483
appear_count <- Appearances %>% 
  filter(yearID >= 2005) %>% 
  select(-c(yearID, teamID, lgID, playerID, G_all, GS, G_batting, G_defense))
primary.position <- colnames(appear_count)[max.col(appear_count)]

cleaned.data <- Appearances %>% 
  filter(yearID >= 2005) %>% 
  select(c(yearID, teamID, lgID, playerID, G_all, GS, G_batting, G_defense)) %>%
  cbind(primary.position)

#Take out pinch hitter and pinch runner
clean.data.appear <- cleaned.data[-which(cleaned.data$primary.position == "G_pr" | cleaned.data$primary.position == "G_ph"),]

People %>% select(c(yearID, playerID, weight, height, bats))


#Create age variable
battingStats() %>%
  left_join(salaries,
            by =c("playerID", "yearID", "teamID")) %>%
  left_join(peopleInfo, by = "playerID") %>%
  mutate(age = yearID - birthYear -
           1L *(birthMonth >= 10)) %>%
  arrange(playerID, yearID, stint)

battingStats()

#predictive factors
#People %>% weight; weight of player
#People %>% height; height of player
#Consider BMI
#Consider Age
#People %>% bats: player's batting and L, R, or B (Both)
#Consider flyball percentage
#Consider exit velocity
#Consider launch angle; intersection of exit velo and launch angle


unique(cleaned.data[-which(cleaned.data$primary.position == "G_pr" | cleaned.data$primary.position == "G_ph"),]$primary.position)


career
Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>% group_by(playerID)

#Include names
People %>%
  as_tibble() %>%
  dplyr::select(playerID, nameFirst, nameLast)%>% 
  unite(name, nameFirst, nameLast, sep = " ")%>% 
  inner_join(career, by = "playerID")





#Reading in Data:
setwd("/Users/garyzhou/Downloads/")
data <- read.table("hitters.csv",header=T,sep=",",row.names=NULL)
hr <- data$HR
ab <- data$AB
year <- data$year
player <- data$name

#Calculating homerun proportion:
hrprop <- hr/ab
hist(hrprop)
Y <- hrprop
n <- length(Y)

sbprop <- data$SB/ab
hist(sbprop)

# starting point
alpha <- 0.5
mu0 <- 0.01
mu1 <- 0.1
sigsq0 <- 0.001
sigsq1 <- 0.001

# gibbs sampler
numiters <- 2000
params <- matrix(NA,nrow=numiters,ncol=5)
params[1,] <- c(alpha,mu0,mu1,sigsq0,sigsq1)
inds <- matrix(NA,nrow=numiters-1,ncol=n)
for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA,n)
  for (j in 1:n){
    a <- dnorm(Y[j],mu1,sqrt(sigsq1))*alpha    
    b <- dnorm(Y[j],mu0,sqrt(sigsq0))*(1-alpha)
    p <- a/(a+b)
    I[j] <- rbinom(1,1,p)
  }
  ## calculating statistics from indicator variables
  n0 <- sum(I==0)
  n1 <- sum(I==1)
  meanY0 <- mean(Y[I==0])
  meanY1 <- mean(Y[I==1])
  ## sampling alphas
  alpha <- rbeta(1,n1+1,n0+1)
  ## sampling means
  mu0 <- rnorm(1,meanY0,sqrt(sigsq0/n0))
  mu1 <- rnorm(1,meanY1,sqrt(sigsq1/n1))
  ## calculating statistics from new means 
  ss0 <- sum((Y[I==0]-mu0)^2)
  ss1 <- sum((Y[I==1]-mu1)^2)
  ## sampling variances
  temp <- rgamma(1,shape=(n0/2)+1,rate=ss0/2)
  sigsq0 <- 1/temp
  temp <- rgamma(1,shape=(n1/2)+1,rate=ss1/2)
  sigsq1 <- 1/temp
  ## storing current values
  params[i,] <- c(alpha,mu0,mu1,sigsq0,sigsq1)
  inds[i-1,] <- I
  print(i)
  #print(round(c(alpha,mu0,mu1,sigsq0,sigsq1),5))
}

params.data1 <- params 
params.data1
#running Gibbs sampler for alternate starting point with code above

alpha <- 0.80
mu0 <- 0.03
mu1 <- 0.05
sigsq0 <- 0.005
sigsq1 <- 0.005

params.data2 <- params 
params.data2
# comparing iterations between two chains 
par(mfrow=c(1,1))
plot(1:numiters, params.data2[,1], main="alpha", type="l", col=3, xlab="Number of Iterations", ylab="alpha")
lines(1:numiters,params.data1[,1],col=2)

par(mfrow=c(3,2))
ymin <- min(params.data2[,1],params.data1[,1])
ymax <- max(params.data2[,1],params.data1[,1])
plot(1:numiters,params.data2[,1],main="alpha",type="l",col=3,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="alpha")
lines(1:numiters,params.data1[,1],col=2)

ymin <- min(params.data2[,2],params.data1[,2])
ymax <- max(params.data2[,2],params.data1[,2])
plot(1:numiters,params.data2[,2],main="mu1",type="l",col="green",ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu1")
lines(1:numiters,params.data1[,2],col=2)
ymin <- min(params.data2[,3],params.data1[,3])
ymax <- max(params.data2[,3],params.data1[,3])
plot(1:numiters,params.data2[,3],main="mu2",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu2")
lines(1:numiters,params.data1[,3],col=2)
ymin <- min(params.data2[,4],params.data1[,4])
ymax <- max(params.data2[,4],params.data1[,4])
plot(1:numiters,params.data2[,4],main="sigsq1",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq1")
lines(1:numiters,params.data1[,4],col=2)
ymin <- min(params.data2[,5],params.data1[,5])
ymax <- max(params.data2[,5],params.data1[,5])
plot(1:numiters,params.data2[,5],main="sigsq2",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq2")
lines(1:numiters,params.data1[,5],col=2)

params.data1.postburn<-params.data1[201:2000,]
params.data2.postburn<-params.data2[201:2000,]

par(mfrow=c(3,2))
acf(params.data1.postburn[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.postburn[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.postburn[,2],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.postburn[,2],lag.max=100,main="ACF: mu1, chain 2")
acf(params.data1.postburn[,3],lag.max=100,main="ACF: mu2, chain 1")
acf(params.data2.postburn[,3],lag.max=100,main="ACF: mu2, chain 2")

par(mfrow=c(2,2))
acf(params.data1.postburn[,4],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.postburn[,4],lag.max=100,main="ACF: sigsq1, chain 2")
acf(params.data1.postburn[,5],lag.max=100,main="ACF: sigsq2, chain 1")
acf(params.data2.postburn[,5],lag.max=100,main="ACF: sigsq2, chain 2")

# taking only every fiftieth draw 
temp <- 50*c(1:(1800/50))

params.data1.thinned <- params.data1.postburn[temp,]
params.data2.thinned <- params.data2.postburn[temp,]

par(mfrow=c(3,2))
acf(params.data1.thinned[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.thinned[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.thinned[,2],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.thinned[,2],lag.max=100,main="ACF: mu1, chain 2")
acf(params.data1.thinned[,3],lag.max=100,main="ACF: mu2, chain 1")
acf(params.data2.thinned[,3],lag.max=100,main="ACF: mu2, chain 2")

par(mfrow=c(2,2))
acf(params.data1.thinned[,4],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.thinned[,4],lag.max=100,main="ACF: sigsq1, chain 2")
acf(params.data1.thinned[,5],lag.max=100,main="ACF: sigsq2, chain 1")
acf(params.data2.thinned[,5],lag.max=100,main="ACF: sigsq2, chain 2")

# combining chains and calculating posterior intervals
params.final <- rbind(params.data1.thinned,params.data2.thinned)

numsamples <- length(params.final[,1])
alpha.gibbs <- params.final[,1]
mu0.gibbs <- params.final[,2]
mu1.gibbs <- params.final[,3]
sigsq0.gibbs <- params.final[,4]
sigsq1.gibbs <- params.final[,5]

par(mfrow=c(3,2))
hist(mu0.gibbs, main="mu1")
hist(mu1.gibbs, main="mu2")
hist(sigsq0.gibbs, main="sigsq1")
hist(sigsq1.gibbs, main="sigsq2")
hist(alpha.gibbs, main="alpha")

summary(mu0.gibbs)
summary(mu1.gibbs)
summary(sigsq0.gibbs)
summary(sigsq1.gibbs)
summary(alpha.gibbs)

# fitted densities
par(mfrow=c(1,1))
hist(hrprop,main="Home Run Proportions",xlab="Home Run Proportions",col="gray")
numY <- length(Y)
xpoints <-ppoints(1000)*0.15
for (i in 1:numsamples){
  ylines <- 100*(alpha.gibbs[i]*dnorm(xpoints,mu1.gibbs[i],sqrt(sigsq1.gibbs[i])) + (1-alpha.gibbs[i])*dnorm(xpoints,mu0.gibbs[i],sqrt(sigsq0.gibbs[i])))
  lines(xpoints,ylines,col="blue")
}

hist(hrprop,main="Home Run Proportions",xlab="Home Run Proportions",col="gray")
y1 <- (alpha)*dnorm(xpoints, mu1.gibbs, sqrt(sigsq1.gibbs))
lines(xpoints, y1, col="green")

#Individual HR Probabilities for Players
prob.elite.hrprop <- Estep(hrprop, alpha, mu1, mu2, sigsq)
#Find out the playerIDs of player seasons that have a greater than 50% chance eliteplayernames <- playerID[which(prob.elite.hrprop > 0.50)]
#Find out the years of player seasons that have a greater than 50% chance of eliteplayeryears <- yearID[which(prob.elite.hrprop > 0.50)]
#Find out the home run rates and totals of player seasons that have a greater eliteplayerhrprop <- hrprop[which(prob.elite.hrprop > 0.50)]
eliteplayerhrtot <- HR[which(mu1.gibbs > 0.50)]
hr[which(mu1.gibbs > 0.50)]
length(mu1.gibbs)


feliteplayerhrtot <- HR[which(mu > 0.50)]
#Get the probability of being in the elite group for player seasons we've ide prob.elite <- Estep(eliteplayerhrprop, alpha, mu1, mu2, sigsq)
#Combine columns and put into table
elite.hrhitters <- cbind(eliteplayernames, eliteplayeryears, prob.elite, elit final_table <- xtable(elite.hrhitters)
                         names(final_table) <- c("Player Names", "Year", "Elite Group Probability", "H #Filter out those players who have had seasons of elite home run hitting final_table %>% arrange(desc(prob.elite)) %>% count()
final_table %>% arrange(desc(prob.elite)) %>% head(5)
final_table %>% arrange(prob.elite) %>% head(5)



##############################################################################################################################
#Calculating BA:

BA <- data$H/data$AB
hist(BA)
Y <- BA
n <- length(Y)

# starting point
alpha <- 0.5
mu0 <- 0.1
mu1 <- 0.4
sigsq0 <- 0.001
sigsq1 <- 0.001

# gibbs sampler
numiters <- 2000
params <- matrix(NA,nrow=numiters,ncol=5)
params[1,] <- c(alpha,mu0,mu1,sigsq0,sigsq1)
inds <- matrix(NA,nrow=numiters-1,ncol=n)
for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA,n)
  for (j in 1:n){
    a <- dnorm(Y[j],mu1,sqrt(sigsq1))*alpha    
    b <- dnorm(Y[j],mu0,sqrt(sigsq0))*(1-alpha)
    p <- a/(a+b)
    I[j] <- rbinom(1,1,p)
  }
  ## calculating statistics from indicator variables
  n0 <- sum(I==0)
  n1 <- sum(I==1)
  meanY0 <- mean(Y[I==0])
  meanY1 <- mean(Y[I==1])
  ## sampling alphas
  alpha <- rbeta(1,n1+1,n0+1)
  ## sampling means
  mu0 <- rnorm(1,meanY0,sqrt(sigsq0/n0))
  mu1 <- rnorm(1,meanY1,sqrt(sigsq1/n1))
  ## calculating statistics from new means 
  ss0 <- sum((Y[I==0]-mu0)^2)
  ss1 <- sum((Y[I==1]-mu1)^2)
  ## sampling variances
  temp <- rgamma(1,shape=(n0/2)+1,rate=ss0/2)
  sigsq0 <- 1/temp
  temp <- rgamma(1,shape=(n1/2)+1,rate=ss1/2)
  sigsq1 <- 1/temp
  ## storing current values
  params[i,] <- c(alpha,mu0,mu1,sigsq0,sigsq1)
  inds[i-1,] <- I
  print(i)
  #print(round(c(alpha,mu0,mu1,sigsq0,sigsq1),5))
}

params.data1 <- params 
params.data1
#running Gibbs sampler for alternate starting point with code above

alpha <- 0.80
mu0 <- 0.2
mu1 <- 0.3
sigsq0 <- 0.005
sigsq1 <- 0.005

params.data2 <- params 
params.data2
# comparing iterations between two chains 

par(mfrow=c(3,2))
ymin <- min(params.data2[,1],params.data1[,1])
ymax <- max(params.data2[,1],params.data1[,1])
plot(1:numiters,params.data2[,1],main="alpha",type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,params.data1[,1],col=3)
ymin <- min(params.data2[,2],params.data1[,2])
ymax <- max(params.data2[,2],params.data1[,2])
plot(1:numiters,params.data2[,2],main="mu0",type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,params.data1[,2],col=3)
ymin <- min(params.data2[,3],params.data1[,3])
ymax <- max(params.data2[,3],params.data1[,3])
plot(1:numiters,params.data2[,3],main="mu1",type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,params.data1[,3],col=3)
ymin <- min(params.data2[,4],params.data1[,4])
ymax <- max(params.data2[,4],params.data1[,4])
plot(1:numiters,params.data2[,4],main="sigsq0",type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,params.data1[,4],col=3)
ymin <- min(params.data2[,5],params.data1[,5])
ymax <- max(params.data2[,5],params.data1[,5])
plot(1:numiters,params.data2[,5],main="sigsq1",type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,params.data1[,5],col=3)

params.data1.postburn<-params.data1[201:2000,]
params.data2.postburn<-params.data2[201:2000,]

par(mfrow=c(3,2))
acf(params.data1.postburn[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.postburn[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.postburn[,2],lag.max=100,main="ACF: mu0, chain 1")
acf(params.data2.postburn[,2],lag.max=100,main="ACF: mu0, chain 2")
acf(params.data1.postburn[,3],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.postburn[,3],lag.max=100,main="ACF: mu1, chain 2")

par(mfrow=c(2,2))
acf(params.data1.postburn[,4],lag.max=100,main="ACF: sigsq0, chain 1")
acf(params.data2.postburn[,4],lag.max=100,main="ACF: sigsq0, chain 2")
acf(params.data1.postburn[,5],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.postburn[,5],lag.max=100,main="ACF: sigsq1, chain 2")

# taking only every fiftieth draw 
temp <- 50*c(1:(1800/50))

params.data1.thinned <- params.data1.postburn[temp,]
params.data2.thinned <- params.data2.postburn[temp,]

par(mfrow=c(3,2))
acf(params.data1.thinned[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.thinned[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.thinned[,2],lag.max=100,main="ACF: mu0, chain 1")
acf(params.data2.thinned[,2],lag.max=100,main="ACF: mu0, chain 2")
acf(params.data1.thinned[,3],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.thinned[,3],lag.max=100,main="ACF: mu1, chain 2")

par(mfrow=c(2,2))
acf(params.data1.thinned[,4],lag.max=100,main="ACF: sigsq0, chain 1")
acf(params.data2.thinned[,4],lag.max=100,main="ACF: sigsq0, chain 2")
acf(params.data1.thinned[,5],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.thinned[,5],lag.max=100,main="ACF: sigsq1, chain 2")

# combining chains and calculating posterior intervals
params.final <- rbind(params.data1.thinned,params.data2.thinned)

numsamples <- length(params.final[,1])
alpha.gibbs <- params.final[,1]
mu0.gibbs <- params.final[,2]
mu1.gibbs <- params.final[,3]
sigsq0.gibbs <- params.final[,4]
sigsq1.gibbs <- params.final[,5]

par(mfrow=c(3,2))
hist(mu0.gibbs)
hist(mu1.gibbs)
hist(sigsq0.gibbs)
hist(sigsq1.gibbs)
hist(alpha.gibbs)

# fitted densities
par(mfrow=c(1,1))
hist(BA,main="Batting Average",xlab="y",col="gray")
numY <- length(Y)
xpoints <-ppoints(1000)*0.15
for (i in 1:numsamples){
  ylines <- 100*(alpha.gibbs[i]*dnorm(xpoints,mu1.gibbs[i],sqrt(sigsq1.gibbs[i])) + (1-alpha.gibbs[i])*dnorm(xpoints,mu0.gibbs[i],sqrt(sigsq0.gibbs[i])))
  lines(xpoints,ylines,col="red")
}

f")
                         

###################################################################################
Calculating SB:
sbproportion <- data$SB/data$AB
hist(sbproportion)
Y <- sbproportion
n <- length(Y)

# starting point
alpha <- 0.5
mu0 <- 0.01
mu1 <- 0.3
sigsq0 <- 0.001
sigsq1 <- 0.001

# gibbs sampler
numiters <- 2000
params <- matrix(NA,nrow=numiters,ncol=5)
params[1,] <- c(alpha,mu0,mu1,sigsq0,sigsq1)
inds <- matrix(NA,nrow=numiters-1,ncol=n)
for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA,n)
  for (j in 1:n){
    a <- dnorm(Y[j],mu1,sqrt(sigsq1))*alpha    
    b <- dnorm(Y[j],mu0,sqrt(sigsq0))*(1-alpha)
    p <- a/(a+b)
    I[j] <- rbinom(1,1,p)
  }
  ## calculating statistics from indicator variables
  n0 <- sum(I==0)
  n1 <- sum(I==1)
  meanY0 <- mean(Y[I==0])
  meanY1 <- mean(Y[I==1])
  ## sampling alphas
  alpha <- rbeta(1,n1+1,n0+1)
  ## sampling means
  mu0 <- rnorm(1,meanY0,sqrt(sigsq0/n0))
  mu1 <- rnorm(1,meanY1,sqrt(sigsq1/n1))
  ## calculating statistics from new means 
  ss0 <- sum((Y[I==0]-mu0)^2)
  ss1 <- sum((Y[I==1]-mu1)^2)
  ## sampling variances
  temp <- rgamma(1,shape=(n0/2)+1,rate=ss0/2)
  sigsq0 <- 1/temp
  temp <- rgamma(1,shape=(n1/2)+1,rate=ss1/2)
  sigsq1 <- 1/temp
  ## storing current values
  params[i,] <- c(alpha,mu0,mu1,sigsq0,sigsq1)
  inds[i-1,] <- I
  print(i)
  #print(round(c(alpha,mu0,mu1,sigsq0,sigsq1),5))
}

params.data1 <- params 
params.data1
#running Gibbs sampler for alternate starting point with code above

alpha <- 0.90
mu0 <- 0.05
mu1 <- 0.2
sigsq0 <- 0.01
sigsq1 <- 0.01

params.data2 <- params 
params.data2
# comparing iterations between two chains 

par(mfrow=c(3,2))
ymin <- min(params.data2[,1],params.data1[,1])
ymax <- max(params.data2[,1],params.data1[,1])
plot(1:numiters,params.data2[,1],main="alpha",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="alpha")
lines(1:numiters,params.data1[,1],col=3)
ymin <- min(params.data2[,2],params.data1[,2])
ymax <- max(params.data2[,2],params.data1[,2])
plot(1:numiters,params.data2[,2],main="mu1",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu1")
lines(1:numiters,params.data1[,2],col=3)
ymin <- min(params.data2[,3],params.data1[,3])
ymax <- max(params.data2[,3],params.data1[,3])
plot(1:numiters,params.data2[,3],main="mu2",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu2")
lines(1:numiters,params.data1[,3],col=3)
ymin <- min(params.data2[,4],params.data1[,4])
ymax <- max(params.data2[,4],params.data1[,4])
plot(1:numiters,params.data2[,4],main="sigsq1",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq1")
lines(1:numiters,params.data1[,4],col=3)
ymin <- min(params.data2[,5],params.data1[,5])
ymax <- max(params.data2[,5],params.data1[,5])
plot(1:numiters,params.data2[,5],main="sigsq2",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq2")
lines(1:numiters,params.data1[,5],col=3)

params.data1.postburn<-params.data1[201:2000,]
params.data2.postburn<-params.data2[201:2000,]

par(mfrow=c(3,2))
acf(params.data1.postburn[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.postburn[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.postburn[,2],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.postburn[,2],lag.max=100,main="ACF: mu1, chain 2")
acf(params.data1.postburn[,3],lag.max=100,main="ACF: mu2, chain 1")
acf(params.data2.postburn[,3],lag.max=100,main="ACF: mu2, chain 2")

par(mfrow=c(2,2))
acf(params.data1.postburn[,4],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.postburn[,4],lag.max=100,main="ACF: sigsq1, chain 2")
acf(params.data1.postburn[,5],lag.max=100,main="ACF: sigsq2, chain 1")
acf(params.data2.postburn[,5],lag.max=100,main="ACF: sigsq2, chain 2")

# taking only every fiftieth draw 
temp <- 50*c(1:(1800/50))

params.data1.thinned <- params.data1.postburn[temp,]
params.data2.thinned <- params.data2.postburn[temp,]

par(mfrow=c(3,2))
acf(params.data1.thinned[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.data2.thinned[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.data1.thinned[,2],lag.max=100,main="ACF: mu1, chain 1")
acf(params.data2.thinned[,2],lag.max=100,main="ACF: mu1, chain 2")
acf(params.data1.thinned[,3],lag.max=100,main="ACF: mu2, chain 1")
acf(params.data2.thinned[,3],lag.max=100,main="ACF: mu2, chain 2")

par(mfrow=c(2,2))
acf(params.data1.thinned[,4],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.data2.thinned[,4],lag.max=100,main="ACF: sigsq1, chain 2")
acf(params.data1.thinned[,5],lag.max=100,main="ACF: sigsq2, chain 1")
acf(params.data2.thinned[,5],lag.max=100,main="ACF: sigsq2, chain 2")

# combining chains and calculating posterior intervals
params.final <- rbind(params.data1.thinned,params.data2.thinned)

numsamples <- length(params.final[,1])
alpha.gibbs <- params.final[,1]
mu0.gibbs <- params.final[,2]
mu1.gibbs <- params.final[,3]
sigsq0.gibbs <- params.final[,4]
sigsq1.gibbs <- params.final[,5]

par(mfrow=c(3,2))
hist(mu0.gibbs,xlab="mu1", main="mu1")
hist(mu1.gibbs, xlab="mu2", main="mu2")
hist(sigsq0.gibbs, xlab="sigsq1", main="sigsq1")
hist(sigsq1.gibbs, xlab="sigsq2", main="sigsq2")
hist(alpha.gibbs, xlab="alpha", main="alpha")

# fitted densities
par(mfrow=c(1,1))
hist(sbproportion,main="Stolen Base Proportion",xlab="y",col="gray")
numY <- length(Y)
xpoints <-ppoints(1000)*0.15
for (i in 1:numsamples){
  ylines <- 100*(alpha.gibbs[i]*dnorm(xpoints,mu1.gibbs[i],sqrt(sigsq1.gibbs[i])) + (1-alpha.gibbs[i])*dnorm(xpoints,mu0.gibbs[i],sqrt(sigsq0.gibbs[i])))
  lines(xpoints,ylines,col="blue")
}






# Create a matrix from data
college_mat <- model.matrix(MD_EARN_WNE_P10~., collegescorecard)
dim(college_mat)
head(college_mat)
collegescore_mat <- college_mat[,2:ncol(college_mat)]

# Split into test and train 
ids_train <- sample.int(nrow(collegescorecard), size = 0.75*nrow(collegescorecard))
train_x <- collegescore_mat[ids_train,]
train_y <- collegescorecard$MD_EARN_WNE_P10[ids_train]
test_x <- collegescore_mat[-ids_train,] 
test_y <- collegescorecard$MD_EARN_WNE_P10[-ids_train]

# Scale
train_means <- apply(train_x, 2, mean)
train_sds <- apply(train_x, 2, sd)
train_x <- sweep(sweep(train_x, 2L, train_means), 2, train_sds, "/")
test_x <- sweep(sweep(test_x, 2L, train_means), 2, train_sds, "/")
dim(train_x)
head(train_x)

#Neural Network Median Income
model_nn <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(75)) %>%
  #layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  #layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(model_nn) # How many parameters are there?

# Train using validation set and early stopping
model_nn %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

history <- model_nn %>% 
  fit(
    x = train_x,
    y = train_y,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(history)

RSS <- sum((predict(model_nn, test_x) - test_y)^2)
TSS <- sum((test_y - mean(test_y))^2)
rsq <- 1-RSS/TSS
rsq

###############################################
#Neural Network with Dropouts
model_nn <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(75)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(model_nn) # How many parameters are there?

# Train using validation set and early stopping
model_nn %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

history <- model_nn %>% 
  fit(
    x = train_x,
    y = train_y,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(history)

RSS <- sum((predict(model_nn, test_x) - test_y)^2)
TSS <- sum((test_y - mean(test_y))^2)
rsq <- 1-RSS/TSS
rsq

#########################################################################################################

#Log Median Income
logcollegescorecard <- collegescorecard
logcollegescorecard$MD_EARN_WNE_P10 <- log(logcollegescorecard$MD_EARN_WNE_P10)

hist(logcollegescorecard$MD_EARN_WNE_P10)
hist(collegescorecard$MD_EARN_WNE_P10)

# Create a matrix from data
logcollege_mat <- model.matrix(MD_EARN_WNE_P10~., logcollegescorecard)
logcollegescore_mat <- logcollege_mat[,2:ncol(logcollege_mat)]

# Split into test and train 
logids_train <- sample.int(nrow(logcollegescorecard), size = 0.75*nrow(logcollegescorecard))
logtrain_x <- logcollegescore_mat[logids_train,]
logtrain_y <- logcollegescorecard$MD_EARN_WNE_P10[logids_train]
logtest_x <- logcollegescore_mat[-logids_train,] 
logtest_y <- logcollegescorecard$MD_EARN_WNE_P10[-logids_train]

# Scale
logtrain_means <- apply(logtrain_x, 2, mean)
logtrain_sds <- apply(logtrain_x, 2, sd)
logtrain_x <- sweep(sweep(logtrain_x, 2L, logtrain_means), 2, logtrain_sds, "/")
logtest_x <- sweep(sweep(logtest_x, 2L, logtrain_means), 2, logtrain_sds, "/")
dim(logtrain_x)

#Neural Net Log Median Income 
logmodel_nn <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(75)) %>%
  #layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  #layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(logmodel_nn) # How many parameters are there?

# Train using validation set and early stopping
logmodel_nn %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

loghistory <- logmodel_nn %>% 
  fit(
    x = logtrain_x,
    y = logtrain_y,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(loghistory)

logRSS <- sum((predict(logmodel_nn, logtest_x) - logtest_y)^2)
logTSS <- sum((logtest_y - mean(logtest_y))^2)
logrsq <- 1-logRSS/logTSS
logrsq

##############################################
#Neural Net Log Median Income and Dropouts
logmodel_nn <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(75)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(logmodel_nn) # How many parameters are there?

# Train using validation set and early stopping
logmodel_nn %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

loghistory <- logmodel_nn %>% 
  fit(
    x = logtrain_x,
    y = logtrain_y,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(loghistory)

logRSS <- sum((predict(logmodel_nn, logtest_x) - logtest_y)^2)
logTSS <- sum((logtest_y - mean(logtest_y))^2)
logrsq <- 1-logRSS/logTSS
logrsq



#################################################
#Neural Network with Dropouts
model_nn <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(75)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(model_nn) # How many parameters are there?

# Train using validation set and early stopping
model_nn %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

history <- model_nn %>% 
  fit(
    x = train_x,
    y = train_y,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(history)

RSS <- sum((predict(model_nn, test_x) - test_y)^2)
TSS <- sum((test_y - mean(test_y))^2)
rsq <- 1-RSS/TSS
rsq

###################################################################################################################
#Remove Financial Variables
collegescorecard_nofinance <- subset(collegescorecard, select = -c(MEDIAN_HH_INC, POVERTY_RATE, UNEMP_RATE))
dim(collegescorecard_nofinance)

# Create a matrix from data
college_mat_nofinance <- model.matrix(MD_EARN_WNE_P10~., collegescorecard_nofinance)
head(college_mat_nofinance)
collegescore_mat_nofinance <- college_mat_nofinance[,2:ncol(college_mat_nofinance)]

# Split into test and train 
ids_train_nofinance <- sample.int(nrow(collegescorecard_nofinance), size = 0.75*nrow(collegescorecard_nofinance))
train_x_nofinance <- collegescore_mat_nofinance[ids_train_nofinance,]
train_y_nofinance <- collegescorecard_nofinance$MD_EARN_WNE_P10[ids_train_nofinance]
test_x_nofinance <- collegescore_mat_nofinance[-ids_train_nofinance,] 
test_y_nofinance <- collegescorecard_nofinance$MD_EARN_WNE_P10[-ids_train_nofinance]
dim(train_x_nofinance)

# Scale
train_means_nofinance <- apply(train_x_nofinance, 2, mean)
train_sds_nofinance <- apply(train_x_nofinance, 2, sd)
train_x_nofinance <- sweep(sweep(train_x_nofinance, 2L, train_means_nofinance), 2, train_sds_nofinance, "/")
test_x_nofinance <- sweep(sweep(test_x_nofinance, 2L, train_means_nofinance), 2, train_sds_nofinance, "/")
dim(train_x_nofinance)
head(train_x_nofinance)

#Neural Network Median Income No Dropouts No Finance
model_nn_nofinance <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(72)) %>%
  #layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  #layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(model_nn_nofinance) # How many parameters are there?

# Train using validation set and early stopping
model_nn_nofinance %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

history_nofinance <- model_nn_nofinance %>% 
  fit(
    x = train_x_nofinance,
    y = train_y_nofinance,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(history_nofinance)

RSS_nofinance <- sum((predict(model_nn_nofinance, test_x_nofinance) - test_y_nofinance)^2)
TSS_nofinance <- sum((test_y_nofinance - mean(test_y_nofinance))^2)
rsq_nofinance <- 1-RSS_nofinance/TSS_nofinance
rsq_nofinance


########################################################
#Neural Network Median Income No Finance
model_nn_nofinance <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(72)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(model_nn_nofinance) # How many parameters are there?

# Train using validation set and early stopping
model_nn_nofinance %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

history_nofinance <- model_nn_nofinance %>% 
  fit(
    x = train_x_nofinance,
    y = train_y_nofinance,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(history_nofinance)

RSS_nofinance <- sum((predict(model_nn_nofinance, test_x_nofinance) - test_y_nofinance)^2)
TSS_nofinance <- sum((test_y_nofinance - mean(test_y_nofinance))^2)
rsq_nofinance <- 1-RSS_nofinance/TSS_nofinance
rsq_nofinance

#Log Median Income without Finance Variables
logcollegescorecard <- collegescorecard
logcollegescorecard$MD_EARN_WNE_P10 <- log(logcollegescorecard$MD_EARN_WNE_P10)

#Remove Financial Variables
logcollegescorecard_nofinance <- subset(logcollegescorecard, select = -c(MEDIAN_HH_INC, POVERTY_RATE, UNEMP_RATE))
dim(logcollegescorecard_nofinance)

# Create a matrix from data
logcollege_mat_nofinance <- model.matrix(MD_EARN_WNE_P10~., logcollegescorecard_nofinance)
head(logcollege_mat_nofinance)
logcollegescore_mat_nofinance <- logcollege_mat_nofinance[,2:ncol(logcollege_mat_nofinance)]

# Split into test and train 
logids_train_nofinance <- sample.int(nrow(logcollegescorecard_nofinance), size = 0.75*nrow(logcollegescorecard_nofinance))
logtrain_x_nofinance <- logcollegescore_mat_nofinance[logids_train_nofinance,]
logtrain_y_nofinance <- logcollegescorecard_nofinance$MD_EARN_WNE_P10[logids_train_nofinance]
logtest_x_nofinance <- logcollegescore_mat_nofinance[-logids_train_nofinance,] 
logtest_y_nofinance <- logcollegescorecard_nofinance$MD_EARN_WNE_P10[-logids_train_nofinance]
dim(logtrain_x_nofinance)

# Scale
logtrain_means_nofinance <- apply(logtrain_x_nofinance, 2, mean)
logtrain_sds_nofinance <- apply(logtrain_x_nofinance, 2, sd)
logtrain_x_nofinance <- sweep(sweep(logtrain_x_nofinance, 2L, logtrain_means_nofinance), 2, logtrain_sds_nofinance, "/")
logtest_x_nofinance <- sweep(sweep(logtest_x_nofinance, 2L, logtrain_means_nofinance), 2, logtrain_sds_nofinance, "/")
dim(logtrain_x_nofinance)
head(logtrain_x_nofinance)

logmodel_nn_nofinance <- keras_model_sequential() %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(72)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 75, activation = "relu")
summary(logmodel_nn_nofinance) # How many parameters are there?

# Train using validation set and early stopping
logmodel_nn_nofinance %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam", 
  metrics = "mae"
)

loghistory_nofinance <- logmodel_nn_nofinance %>% 
  fit(
    x = logtrain_x_nofinance,
    y = logtrain_y_nofinance,
    epochs = 100,
    validation_split = 0.2,
    callbacks = callback_early_stopping(monitor="val_loss", patience = 4)
  )

# Can reproduce plot of metrics
plot(loghistory_nofinance)

logRSS_nofinance <- sum((predict(logmodel_nn_nofinance, logtest_x_nofinance) - logtest_y_nofinance)^2)
logTSS_nofinance <- sum((logtest_y_nofinance - mean(logtest_y_nofinance))^2)
logrsq_nofinance <- 1-logRSS_nofinance/logTSS_nofinance
logrsq_nofinance



#Notes for fitting NN
#if you don’t have a ton of data, the overfitting can and will be worse. That’s why you should try to use a small network with small datasets like this one.








# Evaluate the model
model %>% evaluate(test_x, test_y, verbose = 0)
pred_probs <- predict(model, test_x)

library(Metrics)
auc(test_y, pred_probs)

# Load model and save model
model %>% save_model_tf('my_model')
model <- load_model_tf('my_model')
model %>% evaluate(test_x, test_y, verbose = 0)









# need longer chains!  but takes too long for class, so I did it earlier
load("hitters.gibbs.chain1.Rout")
load("hitters.gibbs.chain2.Rout")
numiters <- 2000

par(mfrow=c(3,2))
ymin <- min(params.chain1[,1],params.chain2[,1])
ymax <- max(params.chain1[,1],params.chain2[,1])
plot(1:numiters,params.chain1[,1],main="alpha",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="alpha")
lines(1:numiters,params.chain2[,1],col=3)
ymin <- min(params.chain1[,2],params.chain2[,2])
ymax <- max(params.chain1[,2],params.chain2[,2])
plot(1:numiters,params.chain1[,2],main="mu1",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu1")
lines(1:numiters,params.chain2[,2],col=3)
ymin <- min(params.chain1[,3],params.chain2[,3])
ymax <- max(params.chain1[,3],params.chain2[,3])
plot(1:numiters,params.chain1[,3],main="mu2",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="mu2")
lines(1:numiters,params.chain2[,3],col=3)
ymin <- min(params.chain1[,4],params.chain2[,4])
ymax <- max(params.chain1[,4],params.chain2[,4])
plot(1:numiters,params.chain1[,4],main="sigsq1",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq1")
lines(1:numiters,params.chain2[,4],col=3)
ymin <- min(params.chain1[,5],params.chain2[,5])
ymax <- max(params.chain1[,5],params.chain2[,5])
plot(1:numiters,params.chain1[,5],main="sigsq2",type="l",col=2,ylim=c(ymin,ymax), xlab="Number of Iterations", ylab="sigsq2")
lines(1:numiters,params.chain2[,5],col=3)

params.chain1.postburn<-params.chain1[201:2000,]
params.chain2.postburn<-params.chain2[201:2000,]

par(mfrow=c(3,2))
acf(params.chain1.postburn[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.chain2.postburn[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.chain1.postburn[,2],lag.max=100,main="ACF: mu0, chain 1")
acf(params.chain2.postburn[,2],lag.max=100,main="ACF: mu0, chain 2")
acf(params.chain1.postburn[,3],lag.max=100,main="ACF: mu1, chain 1")
acf(params.chain2.postburn[,3],lag.max=100,main="ACF: mu1, chain 2")

par(mfrow=c(2,2))
acf(params.chain1.postburn[,4],lag.max=100,main="ACF: sigsq0, chain 1")
acf(params.chain2.postburn[,4],lag.max=100,main="ACF: sigsq0, chain 2")
acf(params.chain1.postburn[,5],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.chain2.postburn[,5],lag.max=100,main="ACF: sigsq1, chain 2")

# taking only every fiftieth draw 
temp <- 50*c(1:(1800/50))

params.chain1.thinned <- params.chain1.postburn[temp,]
params.chain2.thinned <- params.chain2.postburn[temp,]

par(mfrow=c(3,2))
acf(params.chain1.thinned[,1],lag.max=100,main="ACF: alpha, chain 1")
acf(params.chain2.thinned[,1],lag.max=100,main="ACF: alpha, chain 2")
acf(params.chain1.thinned[,2],lag.max=100,main="ACF: mu0, chain 1")
acf(params.chain2.thinned[,2],lag.max=100,main="ACF: mu0, chain 2")
acf(params.chain1.thinned[,3],lag.max=100,main="ACF: mu1, chain 1")
acf(params.chain2.thinned[,3],lag.max=100,main="ACF: mu1, chain 2")

par(mfrow=c(2,2))
acf(params.chain1.thinned[,4],lag.max=100,main="ACF: sigsq0, chain 1")
acf(params.chain2.thinned[,4],lag.max=100,main="ACF: sigsq0, chain 2")
acf(params.chain1.thinned[,5],lag.max=100,main="ACF: sigsq1, chain 1")
acf(params.chain2.thinned[,5],lag.max=100,main="ACF: sigsq1, chain 2")

# combining chains and calculating posterior intervals
params.final <- rbind(params.chain1.thinned,params.chain2.thinned)

numsamples <- length(params.final[,1])
alpha.gibbs <- params.final[,1]
mu0.gibbs <- params.final[,2]
mu1.gibbs <- params.final[,3]
sigsq0.gibbs <- params.final[,4]
sigsq1.gibbs <- params.final[,5]

par(mfrow=c(3,2))
hist(mu0.gibbs)
hist(mu1.gibbs)
hist(sigsq0.gibbs)
hist(sigsq1.gibbs)
hist(alpha.gibbs)

# fitted densities
par(mfrow=c(1,1))
hist(hrprop,main="Batting Average",xlab="y",col="gray")
numY <- length(Y)
xpoints <-ppoints(1000)*0.15
for (i in 1:numsamples){
  ylines <- 100*(alpha.gibbs[i]*dnorm(xpoints,mu1.gibbs[i],sqrt(sigsq1.gibbs[i])) + (1-alpha.gibbs[i])*dnorm(xpoints,mu0.gibbs[i],sqrt(sigsq0.gibbs[i])))
  lines(xpoints,ylines,col="blue")
}

