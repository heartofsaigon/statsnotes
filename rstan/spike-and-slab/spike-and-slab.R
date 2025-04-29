
pacman::p_load(tidyverse, cmdstanr, bayesplot)
set.seed(111)
N = 100

x1 = rnorm(N, 2,6)
x2 = rbinom(N,1, 0.4)
x3= rnorm(N,5,1)

x4 = rgamma(N, 1,7)
x5 = runif(N,1,10)

X = cbind(x1, x2,x3)
y = 0.5 + X%*%c(-1.5, -2, 0.43) + rnorm(N)
X = cbind(X, x4, x5)

mod_dat = list(N = N, X = X, y = c(y), p = ncol(X))

init_fun = function(chain_id){
  set.seed(chain_id)
  list(beta0 = rnorm(1), beta = rnorm(5), sigma = 10)
}

model1 = cmdstanr::cmdstan_model("/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/spike-and-slab/spike-and-slab.stan")
fit1 =  model1$sample(data = mod_dat, chains = 3, parallel_chains = 3, 
                   iter_warmup = 8000, iter_sampling = 3000, refresh = 1000, 
                   max_treedepth = 12, init = init_fun)

fit1$summary("beta", mean, HDInterval::hdi, rhat)
bayesplot::mcmc_trace(fit1$draws("beta"))
fit1$draws("inc_ind", format = "matrix")|>
  apply(2, mean)

### global spike and slab
model1 = cmdstanr::cmdstan_model("/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/spike-and-slab/spike-and-slab-global.stan")
fit1 =  model1$sample(data = mod_dat, chains = 3, parallel_chains = 3, 
                      iter_warmup = 8000, iter_sampling = 3000, refresh = 1000, 
                      max_treedepth = 12, init = init_fun)
fit1$draws("p_inclusion", format = "matrix")|>
  apply(2, \(i) mean(i>0.5) )
fit1$draws("p_inclusion", format = "matrix")|>
  apply(2, mean )
fit1$draws("inc_ind", format = "matrix")|>
  apply(2, mean)
### lasso
model2 = cmdstanr::cmdstan_model("/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/spike-and-slab/lasso.stan")
init_fun = \(chain_id){
  set.seed(chain_id)
  list(
    sigma = rnorm(1,20,0.1),
    sigma_beta = rnorm(1, 20,0.1)
  )
}
fit2 = model2$sample(data = mod_dat, chains = 3, parallel_chains = 3, refresh = 500,
                   iter_sampling = 2000, iter_warmup = 2000, init = init_fun)
fit2$summary("beta", mean, HDInterval::hdi, rhat)

######
# NOTES: a very important note here is that Stan cannot sample discrete 
#parameters like \gamma_j directly, so most implementations in Stan marginalize
# over these latent indicator variables.
# as we know gamma ~ Ber(p); f(beta) = gamma.f(spike) + (1-gamma).f(slab)
# marginalise f(beta) w.r.t to the uncertainty of gamma. Treat f(beta) is a function 
# of gamma. For each case of gamma, what is f(beta)? what is weighted of f(beta)?

#######
init_fun <- function(chain_id) {
  set.seed(chain_id)
  list(
    beta0 = 0,
    beta  = rep(0, 5),
    sigma_beta = rep(1, 5),
    tau = 1,
    sigma = 1
  )
}

model3 = cmdstanr::cmdstan_model("/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/spike-and-slab/horseshoe.stan")
fit3 = model3$sample(data = mod_dat, chains = 3, parallel_chains = 3, 
                   iter_warmup = 3000, iter_sampling = 2000, refresh = 500, 
                   max_treedepth = 15, 
                   adapt_delta = 0.95, init = init_fun)

fit3$summary("beta", mean, HDInterval::hdi, rhat)
bayesplot::mcmc_trace(fit$draws("beta"))

