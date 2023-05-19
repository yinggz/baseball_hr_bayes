library(tidyverse)
library(Lahman)
library(rstan)
library(bayesplot)

batter_2022 <- read.csv("/Users/garyzhou/Downloads/retrosplits-master/daybyday/playing-2022.csv")
str(batter_2022)
batter_2022_df <- batter_2022 %>% filter(season.phase=="R")
test_2022 <- batter_2022 %>% group_by(person.key) %>% summarize(HR=sum(B_HR), AB=sum(B_AB))
head(test_2022)

table(batter_2022$season.phase)


batter_2022_test <- batter_2022_df %>%
  arrange(person.key, game.date) %>%
  group_by(person.key) %>%
  mutate(
    cumulative_AB = cumsum(B_AB),
    cumulative_HR = cumsum(B_HR)
  )

batter_100_AB <- batter_2022_test %>%
  filter(cumulative_AB >= 100) %>%
  group_by(person.key) %>%
  slice(1)

batter_totals <- batter_2022_test %>%
  group_by(person.key) %>%
  summarise(
    total_HR = sum(B_HR),
    total_AB = sum(B_AB)
  )

player_stats <- left_join(batter_totals, batter_100_AB, by = "person.key") %>%
  mutate(
    HR_at_100_AB = cumulative_HR,
    remaining_HR = total_HR - HR_at_100_AB,
    remaining_AB = total_AB - 100
  ) %>%
  select(person.key, HR_at_100_AB, remaining_HR, remaining_AB)

# Replace any negative values with 0
player_stats <- player_stats %>%
  mutate(
    HR_at_100_AB = ifelse(is.na(HR_at_100_AB), 0, HR_at_100_AB),
    remaining_HR = ifelse(is.na(remaining_HR), 0, remaining_HR),
    remaining_AB = ifelse(remaining_AB < 0, 0, remaining_AB)
  )

str(player_stats) 

head(player_stats)

#Remove all non-AB
#Remaining At Bat
#Test
batter_stat_2022 <- player_stats %>%
  filter(remaining_AB >= 100)

batter_stat_2022_final <- batter_stat_2022 %>% mutate(current_AB = 100)

N <- nrow(batter_stat_2022_final)
K <- batter_stat_2022_final$current_AB
y <- batter_stat_2022_final$HR_at_100_AB
K_new <- batter_stat_2022_final$remaining_AB
y_new <- batter_stat_2022_final$remaining_HR
#######################################################
#PA instead of AB
batter_2022 <- read.csv("/Users/garyzhou/Downloads/retrosplits-master/daybyday/playing-2022.csv")
str(batter_2022)
batter_2022_df <- batter_2022 %>% filter(season.phase=="R")
test_2022 <- batter_2022 %>% group_by(person.key) %>% summarize(HR=sum(B_HR), PA=sum(B_PA))
head(test_2022)

table(batter_2022$season.phase)


batter_2022_test <- batter_2022_df %>%
  arrange(person.key, game.date) %>%
  group_by(person.key) %>%
  mutate(
    cumulative_PA = cumsum(B_PA),
    cumulative_HR = cumsum(B_HR)
  )
batter_100_PA <- batter_2022_test %>%
  filter(cumulative_PA >= 170) %>%
  group_by(person.key) %>%
  slice(1)

batter_totals <- batter_2022_test %>%
  group_by(person.key) %>%
  summarise(
    total_HR = sum(B_HR),
    total_PA = sum(B_PA)
  )

player_stats <- left_join(batter_totals, batter_100_PA, by = "person.key") %>%
  mutate(
    HR_at_100_PA = cumulative_HR,
    remaining_HR = total_HR - HR_at_100_PA,
    remaining_PA = total_PA - 170
  ) %>%
  select(person.key, HR_at_100_PA, remaining_HR, remaining_PA)

# Replace any negative values with 0
player_stats <- player_stats %>%
  mutate(
    HR_at_100_PA = ifelse(is.na(HR_at_100_PA), 0, HR_at_100_PA),
    remaining_HR = ifelse(is.na(remaining_HR), 0, remaining_HR),
    remaining_PA = ifelse(remaining_PA < 0, 0, remaining_PA)
  )

str(player_stats) 

head(player_stats)

#Remove all non-AB
#Remaining At Bat
#Test
batter_stat_2022 <- player_stats %>%
  filter(remaining_PA >= 170)

batter_stat_2022_final <- batter_stat_2022 %>% mutate(current_PA = 170)

N <- nrow(batter_stat_2022_final)
K <- batter_stat_2022_final$current_PA
y <- batter_stat_2022_final$HR_at_100_PA
K_new <- batter_stat_2022_final$remaining_PA
y_new <- batter_stat_2022_final$remaining_HR


#######################################################
print(batter_stat_2022_final, n=456)
print(batter_stat_2022_final, n=346)

rstan::rstan_options(mc.cores = parallel::detectCores())

M <- 10000;
fit_pool_2022 <- stan("/Users/garyzhou/Downloads/Hitters1975_PoolBinaryTrials.stan", data=c("N", "K", "y", "K_new", "y_new"),
                 iter=(M / 2), chains=4);
print(fit_pool_2022, c("phi"), probs=c(0.1, 0.5, 0.9));
ss_pool <- rstan::extract(fit_pool_2022)
head(ss_pool)

fit_no_pool_2022 <- stan("no-pool.stan", data=c("N", "K", "y", "K_new", "y_new"),
                    iter=(M / 2), chains=4);
print(fit_no_pool_2022, c("theta"), probs=c(0.1, 0.5, 0.9));
ss_no_pool <- rstan::extract(fit_no_pool_2022)

#Hierarchical Test May 17
fit_hier <- stan("Sluggers_HierBinary_Rankings.stan", data=c("N", "K", "y", "K_new", "y_new"),
                 iter=(M / 2), chains=4,
                 seed=1234,
                 control=list(stepsize=0.01, adapt_delta=0.99));
ss_hier <- rstan::extract(fit_hier)
print(fit_hier, c("theta", "kappa", "phi"), probs=c(0.1, 0.5, 0.9));
print(fit_hier, c("kappa", "phi"), probs=c(0.1, 0.5, 0.9));
print(ss_hier$theta)
means_theta <- apply(ss_hier$theta, 2, mean)
means_theta

save(fit_hier, file = "/Users/garyzhou/Downloads/hier_bayes.rda")

#Hierarchical
fit_hier <- stan("Hitters2022_Hier_Binary.stan", data=c("N", "K", "y", "K_new", "y_new"),
                 iter=(M / 2), chains=4,
                 seed=1234);
ss_hier <- rstan::extract(fit_hier)
print(fit_hier, c("theta", "kappa", "phi"), probs=c(0.1, 0.5, 0.9));
print(fit_hier, c("kappa", "phi"), probs=c(0.1, 0.5, 0.9));
print(ss_hier$theta)
means_theta <- apply(ss_hier$theta, 2, mean)


# Print the mean of each column
print(max(means_theta))

#Trace Plots
mcmc_trace(fit_hier, pars = c("phi", "kappa", "theta[1]", "theta[2]"))
#ACF Plot
mcmc_acf(fit_hier, pars = c("phi", "kappa", "theta[1]", "theta[2]"))

#bayesplot::pp_check(fit_hier, fun="stat_grouped")
pp_check(fit_hier, fun="stat_grouped", stat="mean", nsamples = 100)
pp_check(fit_hier, fun = "stat_grouped", stat = "median")
rstan::pp_check(fit_hier)
#PP Check Test
y_rep_test <- rstan::extract(fit_hier, "y_rep")$y_rep
ppc_dens_overlay(y = y, yrep = y_rep_test)

# Posterior predictive check for the minimum
pp_check(y_rep_test, y = min(y), stat = "min")

# Posterior predictive check for the mean
pp_check(y_rep_test, y = mean(y), stat = "mean")

# Posterior predictive check for the maximum
pp_check(y_rep_test, y = max(y), stat = "max")

# Posterior predictive check for the standard deviation
pp_check(y_rep_test, y = sd(y), stat = "sd")


########Observed vs. Estimated Chance of Success#################3
ss_quantile <- function(ss, N, q) {
  result <- rep(NA, N);
  for (n in 1:N) {
    result[n] <- sort(ss$theta[,n])[M * q];
  }
  return(result);
}

theta_10_hier <- ss_quantile(ss_hier, N, 0.1);
theta_50_hier <- ss_quantile(ss_hier, N, 0.5);
theta_90_hier <- ss_quantile(ss_hier, N, 0.9);

pop_mean <- sum(y) / sum(K);

df_plot2 <- data.frame(x = rep(y / K),
                       y = theta_50_hier,
                       model = rep("partial pooling", N));

plot_bda3_fig_5_4 <-
  ggplot(df_plot2, aes(x=x, y=y)) +
  geom_hline(aes(yintercept=pop_mean), colour="red") +
  geom_abline(intercept=0, slope=1, colour="blue") +
  geom_errorbar(aes(ymin=theta_10_hier,
                    ymax=theta_90_hier),
                width=0.005, colour="gray60") +
  geom_point(colour="gray30", size=0.75) +
  coord_fixed() +
  xlab("observed rate, y[n] / K[n]") +
  ylab("chance of success, theta[n]") +
  ggtitle("Posterior Medians and 80% intervals\n(red line: population mean;  blue line: MLE)")
plot_bda3_fig_5_4;
##################################
#Posterior Predictive Distribution
print(sprintf("%10s  %16.0f", "hier", mean(ss_hier$log_p_new)), quote=FALSE);
log_sum_exp <- function(u) {
  max_u <- max(u);
  a <- 0;
  for (n in 1:length(u)) {
    a <- a + exp(u[n] - max_u);
  }
  return(max_u + log(a));
}

print_new_lp <- function(name, ss) {  
  lp <- -log(M) + log_sum_exp(ss$log_p_new);
  print(sprintf("%25s  %5.1f", name,  lp), quote=FALSE);
}
print_new_lp("partial pooling", ss_hier); 

print(fit_hier, c("z"), probs=c(0.1, 0.5, 0.9), digits=0);

batter_stat_2022_final[109,]
batter_stat_2022_final[5,]


y_new_25_hier <- c(NA, N);
y_new_75_hier <- c(NA, N);
for (n in 1:N) {
  y_new_25_hier[n] <- quantile(ss_hier$z[,n], 0.25)[[1]];
  y_new_75_hier[n] <- quantile(ss_hier$z[,n], 0.75)[[1]];
}

y_new_25_hier <- y_new_25_hier / K_new;
y_new_75_hier <- y_new_75_hier / K_new;

df_post_pred <- data.frame(x = rep(1:N),
                           y = rep(y_new / K_new),
                           model = rep("partial pooling", N));

plot_post_pred <-
  ggplot(df_post_pred, aes(x=x, y=y)) +
  geom_point(colour="darkblue", size=1) +
  geom_errorbar(aes(ymin = y_new_25_hier,
                    ymax = y_new_75_hier),
                width=0.5, colour="gray60") +
  scale_x_continuous(breaks=c()) + 
  xlab("player ID") + 
  ylab("Home Run Rate") +
  ggtitle(expression(
    atop("Posterior Predictions for Home Run Rate (Rest of Season)"
         )));
plot_post_pred;

plot_post_pred <-
  ggplot(df_post_pred, aes(x=x, y=y)) +
  geom_point(colour="darkblue", size=1) +
  geom_errorbar(aes(ymin = y_new_25_hier,
                    ymax = y_new_75_hier),
                width=0.5, colour="gray60") +
  scale_x_continuous(breaks=c()) + 
  xlab("player ID") + 
  ylab("Home Run Rate") +
  ggtitle("Posterior Predictions for Home Run Rate (Rest of Season)");
plot_post_pred

#atop("50% posterior predictive intervals (gray bars); observed (blue dots)", "")


### Rankings ###
print(fit_hier, c("rnk"), probs=c(0.1, 0.5, 0.9));
colMeans(ss_hier$rnk)
sort(colMeans(ss_hier$rnk))
print(fit_hier, c("z"), probs=c(0.1, 0.5, 0.9));
ss_hier$z
summary(ss_hier$y_rep)
summary(ss_hier$y_pop_rep)
print(fit_hier, c("is_best"), probs=c(0.1, 0.5, 0.9));

#Attempt 1 at Fixing Ranking
theta_values <- rstan::extract(fit_hier, "theta")$theta
mean_theta <- colMeans(theta_values)
names(mean_theta) <- 1:N

mean_theta_sorted <- sort(mean_theta, decreasing = TRUE)

rankings <- as.numeric(names(mean_theta_sorted))
print(rankings)

#Find names
names_rankings <- sapply(rankings, function(x)rep(as.character(batter_stat_2022_final[[x,1]])))

#PP HR Remaining
z_values <- rstan::extract(fit_hier, "z")$z
mean_z <- colMeans(z_values)
names(mean_z) <- 1:N

mean_z_sorted <- sort(mean_z, decreasing = TRUE)

rankings_z <- as.numeric(names(mean_z_sorted))
print(rankings_z)
names_rankings_z <- sapply(rankings_z, function(x)rep(as.character(batter_stat_2022_final[[x,1]])))

#print(fit_hier, "rnk", probs=c(0.1, 0.5, 0.9));
df_hr_pp <- data.frame(name = names_rankings,
                            hr_prop = mean_theta_sorted)
df_z_pp <- data.frame(name = names_rankings_z,
                       hr = mean_z_sorted)

df_hr_pp[1:15,]
df_z_pp[1:15,]


hr_total <- df_z_pp$hr
rmse_df <- cbind(as.data.frame(batter_stat_2022_final[rankings_z, ]), hr_total)
sorted_data_hr <- batter_stat_2022_final %>%
  arrange(desc(remaining_HR))
sorted_data_hr[1:15,]
ynew_sorted <- sort(y_new, decreasing = TRUE)

rankings_z <- as.numeric(names(ynew_sorted))

rank_plot <-
  ggplot(df_rank, aes(rank)) +
  stat_count(width=0.8) +
  facet_wrap(~ name) +
  ggtitle("Rankings for Partial Pooling Model");
rank_plot;


sorted_data_hr[which(sorted_data_hr$person.key == "ramij003"),]
sorted_data_hr[which(sorted_data_hr$person.key == "walkc002"),]
sorted_data_hr[which(sorted_data_hr$person.key == "cronc002"),]
sorted_data_hr[which(sorted_data_hr$person.key == "adamw002"),]

rmse_hier <- sqrt(mean((rmse_df$hr_total-rmse_df$remaining_HR)^2))
rmse_hier
df_rank <- data.frame(list(name = rep(as.character(batter_stat_2022_final[[rankings[1],1]]), 1),
                           rank = ss_hier$theta[, rankings[1]]));
for (n in 2:length(names_rankings)) {
  df_rank <- rbind(df_rank,
                   data.frame(list(name = as.character(batter_stat_2022_final[[rankings[n],1]]),
                                   rank = ss_hier$theta[, rankings[n]])));
}








batter_stat_2022_final %>% arrange(desc(HR_at_100_AB))


#Posterior Checks
print(fit_hier, c("min_y_rep", "max_y_rep", "mean_y_rep", "sd_y_rep"), probs=c());

print(fit_hier, c("p_min", "p_max", "p_mean", "p_sd"), probs=c());
y_min <- min(y);
y_max <- max(y);
y_mean <- mean(y);
y_sd <- sd(y);

table(batter_stat_2022_final$HR_at_100_AB)
table(ss_hier$min_y_rep)
table(ss_hier$max_y_rep)
table(ss_hier$mean_y_rep)
table(ss_hier$sd_y_rep)



pvals_frame <- function(ss, model_name) {
  df_pvals_min <- data.frame(list(test_stat = rep("min", M), 
                                  replication = ss$min_y_rep),
                             model = rep(model_name, M));
  df_pvals_max <- data.frame(list(test_stat = rep("max", M), 
                                  replication = ss$max_y_rep),
                             model = rep(model_name, M));
  df_pvals_mean <- data.frame(list(test_stat = rep("mean", M), 
                                   replication = ss$mean_y_rep),    
                              model = rep(model_name, M));
  df_pvals_sd <- data.frame(list(test_stat = rep("sd", M), 
                                 replication = ss$sd_y_rep),
                            model = rep(model_name, M));
  return(rbind(df_pvals_min, df_pvals_max, df_pvals_mean, df_pvals_sd));
}

df_pvals <- rbind(pvals_frame(ss_hier, "partial pool"));

post_test_stat_plot <-
  ggplot(df_pvals, aes(replication)) +
  facet_grid( ~ test_stat) +
  geom_histogram(binwidth = 0.5, colour="black", size = 0.25, fill="white") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("value in replicated data set") +
  geom_vline(aes(xintercept = y_val), 
             data = data.frame(y_val = c(rep(y_min, M), rep(y_max, M), 
                                             rep(y_mean, M), rep(y_sd, M)),
                               test_stat = df_pvals$test_stat,
                               replication = df_pvals$replication),
             colour = "blue", size = 0.25) +
  ggtitle("Posterior p-values")
post_test_stat_plot;


#Replicated Y Values: Graphical Posterior Predictive Checks
print(fit_hier, c("y_rep", "y_pop_rep[1]"), probs=c(0.1, 0.5, 0.9));

#Y Rep
df_post <- data.frame(list(dataset = rep("REAL", N),
                           y = y));
for (n in 1:15) {
  df_post <- rbind(df_post,
                   data.frame(list(dataset = rep(paste("repl ", n), N),
                                   y = ss_hier$y_rep[n,])));
}
post_plot <-
  ggplot(df_post, aes(y)) +
  facet_wrap(~ dataset) +
  stat_count(width=0.8) +
  ggtitle("Existing Item Replication (Beta Prior on Probability)");
post_plot;

#Y Pop Rep
df_post <- data.frame(list(dataset = rep("REAL", N),
                           y = y));
for (n in 1:15) {
  df_post <- rbind(df_post,
                   data.frame(list(dataset = rep(paste("repl ", n), N),
                                   y = ss_hier$y_pop_rep[n,])));
}
post_plot <-
  ggplot(df_post, aes(y)) +
  facet_wrap(~ dataset) +
  stat_count(width=0.8) +
  ggtitle("New Item Replication (Beta Prior on Probability)");
post_plot;






#######################


fit_hier_new <- stan("hier.stan", data=c("N", "K", "y", "K_new", "y_new"),
                 iter=(M / 2), chains=4,
                 seed=1234, 
                 control=list(stepsize=0.01, adapt_delta=0.99));
ss_hier_new <- rstan::extract(fit_hier_new_2022)





#### Test ####
hit_1975 <- read.table("/Users/garyzhou/Downloads/baseball1975_test.txt", header=TRUE)
hit_1975 

hit_1975_df <- with(hit_1975, data.frame(FirstName, LastName, 
                          Hits, At.Bats, 
                          RemainingAt.Bats,
                          RemainingHits = SeasonHits - Hits));
print(hit_1975_df);

N <- dim(hit_1975_df)[1]
K <- hit_1975_df$At.Bats
y <- hit_1975_df$Hits
K_new <- hit_1975_df$RemainingAt.Bats;
y_new <- hit_1975_df$RemainingHits;

M <- 10000;
fit_pool_test <- stan("/Users/garyzhou/Downloads/Hitters1975_PoolBinaryTrials.stan", data=c("N", "K", "y", "K_new", "y_new"),
                 iter=(M / 2), chains=4);
ss_pool <- extract(fit_pool);
fit_pool_test
print(fit_pool_test, c("phi"), probs=c(0.1, 0.5, 0.9));



fit_hier <- stan("Hitters1975_Hier_Binary.stan", data=c("N", "K", "y", "K_new", "y_new"),
                 iter=(M / 2), chains=4,
                 seed=1234,
                 control=list(stepsize=0.01, adapt_delta=0.99));

ss_hier <- rstan::extract(fit_hier);

print(fit_hier, c("theta", "kappa", "phi"), probs=c(0.1, 0.5, 0.9));

#Trace Plots
mcmc_trace(fit_hier, pars = c("phi", "kappa", "theta[1]", "theta[2]"))
#ACF Plot
mcmc_acf(fit_hier, pars = c("phi", "kappa", "theta[1]", "theta[2]"))
ss_quantile <- function(ss, N, q) {
  result <- rep(NA, N);
  for (n in 1:N) {
    result[n] <- sort(ss$theta[,n])[M * q];
  }
  return(result);
}

theta_10_hier <- ss_quantile(ss_hier, N, 0.1);
theta_50_hier <- ss_quantile(ss_hier, N, 0.5);
theta_90_hier <- ss_quantile(ss_hier, N, 0.9);

pop_mean <- sum(y) / sum(K);

df_plot2 <- data.frame(x = rep(y / K),
                       y = theta_50_hier,
                       model = rep("partial pooling", N));

plot_bda3_fig_5_4 <-
  ggplot(df_plot2, aes(x=x, y=y)) +
  geom_hline(aes(yintercept=pop_mean), colour="lightpink") +
  geom_abline(intercept=0, slope=1, colour="skyblue") +
  geom_errorbar(aes(ymin=theta_10_hier,
                    ymax=theta_90_hier),
                width=0.005, colour="gray60") +
  geom_point(colour="gray30", size=0.75) +
  coord_fixed() +
  xlab("observed rate, y[n] / K[n]") +
  ylab("chance of success, theta[n]") +
  ggtitle("Posterior Medians and 80% intervals\n(red line: population mean;  blue line: MLE)")
plot_bda3_fig_5_4;
#Posterior Predictive Distribution
print(sprintf("%10s  %16.0f", "hier", mean(ss_hier$log_p_new)), quote=FALSE);
log_sum_exp <- function(u) {
  max_u <- max(u);
  a <- 0;
  for (n in 1:length(u)) {
    a <- a + exp(u[n] - max_u);
  }
  return(max_u + log(a));
}

print_new_lp <- function(name, ss) {  
  lp <- -log(M) + log_sum_exp(ss$log_p_new);
  print(sprintf("%25s  %5.1f", name,  lp), quote=FALSE);
}
print_new_lp("partial pooling", ss_hier); 

print(fit_hier, c("z"), probs=c(0.1, 0.5, 0.9), digits=0);
y_new_25_hier <- c(NA, N);
y_new_75_hier <- c(NA, N);
for (n in 1:N) {
  y_new_25_hier[n] <- quantile(ss_hier$z[,n], 0.25)[[1]];
  y_new_75_hier[n] <- quantile(ss_hier$z[,n], 0.75)[[1]];
}

y_new_25_hier <- y_new_25_hier / K_new;
y_new_75_hier <- y_new_75_hier / K_new;

df_post_pred <- data.frame(x = rep(1:N),
                           y = rep(y_new / K_new),
                           model = rep("partial pooling", N));

plot_post_pred <-
  ggplot(df_post_pred, aes(x=x, y=y)) +
  geom_point(colour="darkblue", size=1) +
  geom_errorbar(aes(ymin = y_new_25_hier,
                    ymax = y_new_75_hier),
                width=0.5, colour="gray60") +
  scale_x_continuous(breaks=c()) + 
  xlab("player ID") + 
  ylab("Home Run Rate") +
  ggtitle(expression(
    atop("Posterior Predictions for Home Run Rate in Remainder of Season",
         atop("50% posterior predictive intervals (gray bars); observed (blue dots)", ""))));
plot_post_pred;
### Rankings ###
print(fit_hier, c("rnk"), probs=c(0.1, 0.5, 0.9));
ss_hier$rnk
print(fit_hier, c("p_min", "p_max", "p_mean", "p_sd"), probs=c());



fit_hier_logit <- stan("hier-logit.stan", data=c("N", "K", "y", "K_new", "y_new"),
                       iter=(M / 2), chains=4,
                       control=list(stepsize=0.01, adapt_delta=0.99));
ss_hier_logit <- rstan::extract(fit_hier_logit);
print(fit_hier_logit, c("alpha_std", "theta", "mu", "sigma"), probs=c(0.1, 0.5, 0.9));



data("Batting")

# Subset the Batting data for the 2015 baseball season
season_2015 <- subset(Batting, yearID == 2015)

# Group the data by player and calculate cumulative at-bats
season_2015 <- transform(season_2015, CumulativeAtBats = ave(AB, playerID, FUN = cumsum))

# Subset the data for batters at their 100th at-bat
batters_100th_at_bat <- subset(season_2015, CumulativeAtBats >= 100)

str(batters_100th_at_bat)

# Print the extracted stats
print(batters_100th_at_bat)

batter_df <- read.csv("/Users/garyzhou/Downloads/stats.csv");
batter_df <- with(df, data.frame(FirstName, LastName, 
                          Hits, At.Bats, 
                          RemainingAt.Bats,
                          RemainingHits = SeasonHits - Hits));
print(df);

str(batter_df)
table(batter_df$year)

batter_2015df <- read.csv("/Users/garyzhou/Downloads/batter2015.csv")
str(batter_2015df)
summary(batter_2015df$b_total_pa)

batter_df_test <- read.csv("/Users/garyzhou/Downloads/stats (1).csv");
str(batter_df_test)

