
# Set seed for reproducibility
set.seed(123)

# Number of observations
N <- 100

# Simulate a continuous predictor x
x <- rnorm(N)

# Simulate a binary predictor z (e.g., 0 or 1 with probability 0.5)
z <- rbinom(N, size = 1, prob = 0.5)

# True coefficients
beta0 <- 0.5
beta1 <- 2   # effect of the continuous predictor
beta2 <- -3.0   # effect of the binary predictor

# Define thresholds (ensure they are in increasing order)
cut1 <- -0.5
cut2 <- 0.5

# Compute the linear predictor for each observation
eta <- beta0 + beta1 * x + beta2 * z

# Compute probabilities for each ordered outcome using the logistic function
p1 <- plogis(cut1 - eta)                     # P(y = 1)
p2 <- plogis(cut2 - eta) - plogis(cut1 - eta)  # P(y = 2)
p3 <- 1 - plogis(cut2 - eta)                   # P(y = 3)

# Simulate ordered outcome y
y <- sapply(1:N, function(i) {
  sample(1:3, size = 1, prob = c(p1[i], p2[i], p3[i]))
})

# Create a data frame with simulated data
sim_data <- data.frame(x = x, z = z, y = y)
head(sim_data)

mod_dat = list(y = sim_data$y, X = cbind(1, x,z), num_data = N, 
               num_cov = 3, num_group = unique(y)|> length())

model = cmdstanr::cmdstan_model(stan_file = "/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/ordered-logistic/ordered-logistic.stan")

init_fun = function(chain_id){
  set.seed(chain_id)
  list(beta = rnorm(mod_dat$num_cov), thres = rexp(mod_dat$num_group-1,2)|> sort())
}

fit = model$sample(data = mod_dat, chains = 3, parallel_chains = 3, 
                   iter_warmup = 10000, iter_sampling = 3000,
                   refresh = 2000, init = init_fun, adapt_delta = 0.99)

fit$summary("beta",mean, HDInterval::hdi)








