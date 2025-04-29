


pacman::p_load(rstan)

mod_dat = list(y = rexp(100, 2), N = 100)
fit = rstan::stan(file = "exptest.stan", data = mod_dat, chains = 2, iter = 2000)
fit
fit = rstan::stan(file = "exptest2.stan", data = mod_dat, chains = 2, iter = 2000)
a = rstan::extract(fit, pars = "m")
loo::waic(a[[1]])
