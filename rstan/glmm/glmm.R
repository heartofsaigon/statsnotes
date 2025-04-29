
#-------------------
# dat simulation
#-------------------

# Set seed for reproducibility
set.seed(1234)

# Total number of observations and number of groups
N <- 200
J <- 20

# Randomly assign each observation to one of 20 groups
group <- sample(1:J, size = N, replace = TRUE)

# Simulate group-level random intercepts
sigma_b <- 1       # standard deviation for the random intercepts
b <- rnorm(J, mean = 0, sd = sigma_b)

# Fixed effect parameters for the logistic regression
beta0 <- 0.5       # global intercept
beta1 <- 1.0       # coefficient for x1
beta2 <- -0.5      # coefficient for x2
beta3 <- 0.8       # coefficient for x3

# Simulate three covariates from a normal distribution
x1 <- rnorm(N)
x2 <- rpois(N,2)
x3 <- rbinom(N,1,0.6)

# Compute the linear predictor (eta) for each observation:
# eta = beta0 + beta1*x1 + beta2*x2 + beta3*x3 + random intercept from group
eta <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3 + b[group]

# Convert the linear predictor to a probability using the logistic function
p <- 1 / (1 + exp(-eta))

# Simulate binary outcome y from a Bernoulli distribution with probability p
y <- rbinom(N, size = 1, prob = p)

# Combine into a data frame (make group a factor)
data_sim <- data.frame(
  y = y,
  x1 = x1,
  x2 = x2,
  x3 = x3,
  group = factor(group)
)

# Inspect the first few rows
head(data_sim)
###############################################################################
#----------------------
# prepaere model input
#---------------------

model_dat = list(mood = data_sim$y, 
                 X = data_sim[,c("x1","x2","x3")]|> {\(i) cbind(1,i)}(), 
                 region = data_sim$group,
                 N = nrow(data_sim),
                 J = unique(data_sim$group)|> length()
                 )

model_dat$q<- model_dat$X|> ncol()
# using stan --------------------------------------------

model = cmdstanr::cmdstan_model(stan_file = "glmm.stan")
fit = model$sample(data = model_dat, chains = 4, parallel_chains = 4, 
                   iter_warmup = 1500,
                   iter_sampling = 1500,
                   refresh = 1000)
fit$summary(c( "beta"), mean = mean, HDInterval::hdi, rhat)
 # using lmer --------------------------------------------

fit_lm<- lme4::glmer(y ~ x1+x2+x3 + (1|group), 
                     data = data_sim, 
                     family = binomial(link = "logit"))
broom.mixed::tidy(fit_lm, conf.int = T)

# use jags ---------------------------------------------

model_string = textConnection("model{

for(i in 1:N){
mood[i] ~ dbern(p[i])
logit(p[i])<- 
# random effect
   b[region[i]]+
# required
inprod(X[i,],beta[])
}

# prior random
for(i in 1:J){b[i] ~ dnorm(0, tau.b)}
tau.b <- pow(sigma.b,-2)
sigma.b ~ dt(0, 1, 1)T(0,)

# require
for(i in 1:q){beta[i] ~ dnorm(0, 0.01)}

}")

model <- jags.model(model_string, data = model_dat, n.chains=2)

update(model,1500, progress.bar="none")

params_name <- c ( "beta")

samples <- coda.samples(model,
                        variable.names=params_name,
                        n.iter=5000, progress.bar="none")

gelman.diag(samples)

samples[[1]]|>
  apply(2,\(i) c(mean = mean(i), HDInterval::hdi(i)) )|>
  t()





