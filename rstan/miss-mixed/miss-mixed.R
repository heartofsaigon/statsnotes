
set.seed(123)  # For reproducibility

# Define parameters
N <- 100    # Number of subjects
T <- 10     # Number of time points per subject
sigma_e <- 1.0  # Residual standard deviation
sigma_u0 <- 1.5  # Random intercept SD
beta0 <- 5.0    # Fixed intercept
beta1 <- 2.0    # Fixed slope

# Generate subject IDs
subject_id <- rep(1:N, each = T)

# Generate time variable (0 to 9 for each subject)
time <- rep(0:(T-1), N)

# Random intercepts for each subject
u0 <- rnorm(N, 0, sigma_u0)
u0_expanded <- rep(u0, each = T)

# Generate response variable y
y <- beta0 + u0_expanded + beta1 * time + rnorm(N * T, 0, sigma_e)

# Introduce missing values (MAR)
missing_prob <- 0.2  # 20% missing responses
missing_idx <- sample(1:(N * T), size = round(missing_prob * N * T), replace = FALSE)
y[missing_idx] <- NA

# Create dataframe
longitudinal_data <- data.frame(subject_id, time, y)

# Show first few rows
head(longitudinal_data)
################################################################################
################################################################################

ind_obs = which(!is.na(longitudinal_data$y))
ind_miss = which(is.na(longitudinal_data$y))

mod_dat = list(
  y_obs = longitudinal_data$y[ind_obs],
  ID_obs = longitudinal_data$subject_id[ind_obs], 
  time_obs = longitudinal_data$time[ind_obs],
  
  ID_miss = longitudinal_data$subject_id[ind_miss], 
  time_miss = longitudinal_data$time[ind_miss],
  n_sub = longitudinal_data$subject_id|> unique()|> length(),
  n_obs = length(ind_obs),
  n_miss = length(ind_miss)
)


init_fun = function(chain_id){
  set.seed(chain_id)
  list(
    sigma = rnorm(1,20,0.1), sigma_b = rnorm(1,20,0.1),
    beta0 = rnorm(1,0.1,1), beta1 = rnorm(1,0.1,2)
  )
}

model = cmdstanr::cmdstan_model(stan_file = "miss-mixed.stan")
fit = model$sample(mod_dat, chains = 3, parallel_chains = 3, iter_warmup = 2000, 
                   iter_sampling = 2000, refresh = 1000, init = init_fun)

fit$summary(c("beta0", "beta1"))

chain_ids(fit$draws())

bayesplot::mcmc_trace(fit$draws(c("beta0","beta1")))

#####################
################################################

ind_obs = which(!is.na(longitudinal_data$y))
ind_miss = which(is.na(longitudinal_data$y))


mod_dat = list(
  y_obs = longitudinal_data$y[ind_obs],
  ID = longitudinal_data$subject_id, 
  time = longitudinal_data$time,
  
  n_sub = longitudinal_data$subject_id|> unique()|> length(),
  n_obs = length(ind_obs),
  N = nrow(longitudinal_data),
  obs_ind = ind_obs, miss_ind = ind_miss
)

model = cmdstanr::cmdstan_model(stan_file = "miss-mixed2.stan")
fit = model$sample(data = mod_dat, chains = 3, parallel_chains = 3, iter_warmup = 3000,
                   iter_sampling = 2000, refresh = 500)

fit$summary(c("beta0", "beta1"), mean, HDInterval::hdi, rhat)
bayesplot::mcmc_trace(fit$draws(c("beta0", "beta1")))
bayesplot::mcmc_intervals(fit$draws(c("beta0", "beta1")))
