
# install.packages(c("MASS", "mvtnorm"))
library(MASS)
pacman::p_load(tidyverse, HDInterval, posterior, bayesplot)

set.seed(123)

# 1. Parameters
N  <- 50
P  <- 5
beta0 <- 1.5
beta  <- c(2, -1, 0.5, 0, 1)      # true slopes
Sigma_x <- matrix(0.5, nrow=P, ncol=P)
diag(Sigma_x) <- 1               # some correlation among X's
sigma_e <- 1                     # noise SD

# 2. Complete covariates
X_full <- mvrnorm(N, mu=rep(0,P), Sigma=Sigma_x)

# 3. Introduce MCAR missingness (30% per covariate)
X_obs <- X_full
for (j in 1:P) {
  miss_idx <- sample(1:N, size = floor(0.3*N))
  X_obs[miss_idx, j] <- NA
}

# 4. Outcome
#   Use the true X_full so that missingness is independent of Y
y <- beta0 + X_full %*% beta + rnorm(N, 0, sigma_e)

# 5. Prepare indexing for Stan
miss_locs <- which(is.na(X_obs), arr.ind = TRUE)
M         <- nrow(miss_locs)

data_list <- list(
  N        = N,
  P        = P,
  y        = as.vector(y),
  X_obs    = ifelse(is.na(X_obs),0,X_obs),
  M        = M,
  miss_row = miss_locs[,1],
  miss_col = miss_locs[,2]
)

# You can save data_list to file for Stan, e.g.
# rstan::stan_rdump(names(data_list), file="sim_data.Rdump", envir=list2env(data_list))




model = cmdstanr::cmdstan_model("stancode.stan")
fit = model$sample(data = data_list, chains = 3, parallel_chains = 3, refresh = 2000,
                   iter_warmup = 4000, iter_sampling = 2000)
fit$summary(c("beta"), mean, hdi, rhat)







