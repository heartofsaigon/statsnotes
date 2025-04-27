pacman::p_load(tidyverse, lme4, cmdstanr, posterior)

#–– Setup ----
set.seed(2025)

# define true parameters -------------------------------------------------------
# Design
n_per_arm <- 30
times     <- c(0, 4, 6, 12, 52)
ids       <- 1:(2*n_per_arm)

# True parameters
beta0      <- 2.0      # fixed intercept
beta1      <- 1.5      # fixed treatment effect (additive shift)
sd_b0      <- 1.0      # random intercept SD
sd_b1      <- 0.5      # random slope SD
sd_error   <- 1.0     # residual SD
#-------------------------------------------------------------------------------
# Build empty data.frame
df <- expand.grid(
  id        = ids,
  time      = times
)
df$treatment <- ifelse(df$id <= n_per_arm, 0, 1)

# Simulate random effects
rand_eff <- data.frame(
  id    = ids,
  b0    = rnorm(length(ids), 0, sd_b0),
  b1    = rnorm(length(ids), 0, sd_b1)
)
df <- merge(df, rand_eff, by="id")

# Generate outcome
df$y <- beta0 + df$b0 +
  df$b1 * df$time +
  beta1 * df$treatment +
  rnorm(nrow(df), 0, sd_error)

# Force baseline to zero
df<- tibble(df)|> arrange(id, time)

df = dl[[50]]

mod_dat = list(
  N = nrow(df), n = df$id|> unique()|> length(),
  y = df$marker, time = df$times, id = df$id, trt = df$trt
)

model = cmdstanr::cmdstan_model(stan_file = "longitudinal-only.stan")

init_func = \(chain_id){
  set.seed(chain_id)
  list(
    beta   = c(0,0),
    sigma_b = c(1,1),
    sigma   = 1,
    b0 = rnorm(1), b1 = rgamma(1,4,4)
  )
}
fit = model$sample(data = mod_dat, chains = 3, parallel_chains = 3, refresh = 1000,
                   iter_warmup = 2000, iter_sampling = 2000,
                   save_cmdstan_config=TRUE)

fit$summary(c("beta","b_mean"), mean, HDInterval::hdi, rhat)
bayesplot::mcmc_trace(fit$draws(c("beta","b"), format = "matrix")[,sample(mod_dat$n,6)])



#–– Fit linear mixed model ----
m <- lmer(y ~  treatment + (1 + time | id), data = df)
summary(m)









