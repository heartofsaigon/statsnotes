
pacman::p_load(tidyverse, cmdstanr, HDInterval, posterior, bayesplot)

y<-
"46, 31, 37, 62, 30
70, 59
52, 44, 57, 40, 67, 64, 70
47, 21, 70, 46, 14
42, 64, 50, 69, 77, 81, 87
35, 68, 59, 38, 57, 76, 57, 76, 57, 29, 60"

y<-
str_split(y, "\\n")|> 
  unlist()|>
  lapply(\(i) str_split(i,", ", simplify = T))
d<-
map_dfr(1:length(y), \(i){
  tibble(id = i, per = y[[i]]|> as.numeric())
})

stat_dat = list(
  id = d$id, y = d$per, N = nrow(d), J = d$id|> unique()|> length()
)

model = cmdstanr::cmdstan_model("conception.stan")
fit = model$sample(data = stat_dat, chains = 3, parallel_chains = 3, 
                   iter_warmup = 5000, iter_sampling = 2000,
                   adapt_delta = 0.95
                   )

fit$summary(c("gamma", "mu", "sigma_gam", "sigma"), mean, hdi, rhat)

mcmc_trace(fit$draws(c( "mu")))










