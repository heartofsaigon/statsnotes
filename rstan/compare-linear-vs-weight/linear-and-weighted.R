
pacman::p_load(rstan, tidyverse)
N = 500
x = rnorm(N)
y = 0.5 + x*1.2 + rnorm(N)
w = dnorm(sort(rnorm(N,20,0.01)),20, 0.01)

lm(y~x)
lm(y~x, weights = w)

mod_dat = list(y = y, x = x, N = length(y), w = w)
fit = rstan::stan("/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/compare-linear-vs-weight/linear-and-weighted.stan",
                  data = mod_dat, 
                  chains = 2, 
                  iter = 5000)
rstan::summary(fit, pars = "beta")

