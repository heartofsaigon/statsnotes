pacman::p_load(tidyverse)
# Set seed for reproducibility
set.seed(123)

# Define the number of subjects and time points
n_subjects <- 30
n_time <- 4

# Create a data frame with subject IDs and time points
data_long <- expand.grid(subject = 1:n_subjects, time = 1:n_time)

# Simulate a continuous covariate (e.g., normally distributed)
data_long$x <- rnorm(n_subjects * n_time, mean = 0, sd = 1)

# Simulate subject-specific random intercepts to account for within-subject correlation
subject_intercepts <- rnorm(1, mean = 0, sd = 0.5)
data_long$intercept <- subject_intercepts[data_long$subject]

# Specify the logistic regression coefficients:
beta0 <- -1    # fixed intercept
beta1 <- 0.8   # effect of the continuous covariate
beta2 <- 0.3   # effect of time (if desired)

# Calculate the linear predictor (log-odds)
data_long$linear_predictor <- beta0 + beta1 * data_long$x +
  beta2 * data_long$time 

# Convert the linear predictor to probability using the logistic function
data_long$prob <- exp(data_long$linear_predictor) / (1 + exp(data_long$linear_predictor))

# Generate the binary outcome based on the computed probability
data_long$outcome <- rbinom(nrow(data_long), size = 1, prob = data_long$prob)

# Preview the first few rows of the simulated dataset
head(data_long)

dat = data_long|> select(subject, time, x, outcome)

mod_dat = list(y = dat$outcome, x = dat$x, id = dat$subject, N = nrow(dat),
               K = length(unique(dat$subject)), time = dat$time-1
               )

fit = rstan::stan(file = "/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/logistic-mixed-error/logistic-mix-error.stan",
                  data = mod_dat, chains = 2, iter = 1000)
rstan::summary(fit, pars = "beta")[[1]]


lme4::lmer(dat$outcome ~ dat$x + dat$time + (1|dat$subject))

