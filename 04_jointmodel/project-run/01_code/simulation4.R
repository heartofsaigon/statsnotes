
rm(list = ls())

pacman::p_load(tidyverse, lme4, cmdstanr, posterior, msm, beepr)
treatment = rep(c(0,1), each = 150)
lam = 0.05
alpha_trt = -0.5
alpha_mark = 0.8
beta0 = 0.5
beta1 = -1.2
times = seq(0,10, length= 70)|> round(2)|> unique()|> sort()

set.seed(333)

dl<-
  lapply(1:10,\(kk){
    d<-
      lapply(1:length(treatment),\(i){
        b = rnorm(1);
        k = runif(1,1,3)
        A = beta0 + beta1*treatment[i] + b
        U = runif(1)
        R = -log(U)
        event_t = log(1 + alpha_mark*k*R/lam/exp(alpha_mark*A + alpha_trt*treatment[i]))/alpha_mark/k
        
        # times = runif(3, 0, event_t)
        # times = c(times, runif(3,0, event_t*2))|> sort()
        # times = times[times<=event_t]|> round(2)|> unique()
        
        mytimes = times[times<=event_t]
        y = rnorm(length(mytimes), A + k*mytimes,1)
        
        cbind(id = i, trt = treatment[i], marker = y, times = mytimes, event_time = event_t)|>
          as_tibble()#|> mutate(B = (A+k*mytimes))
      })|>
      reduce(bind_rows)
    d = mutate(d, censor =  1*(event_time <= quantile(unique(event_time),0.6)))
    d
  })

sapply(dl, \(i) i$event_time|> max() )|> sort()

k = dl[[1]][,c(1,2,3,4)]
ggplot(k, aes(x = times, y = marker, group = id, color = factor(trt))) +
  geom_line() +
  geom_point() +
  scale_color_manual(
    values = c(
      "0" = "pink",             # fully opaque red
      "1" = alpha("steelblue", 0.4)  # 30% opacity blue
    )
  )+
  theme_minimal() +
  labs(
    title = "Scenario 4",
    x = "Time",
    y = "Longitudinal outcome",
    color = "Treatment"
  )+
  xlim(0,7)




(d = dl[[1]])
d$id|> table()|> sort()

myresult = list()

model = cmdstanr::cmdstan_model(stan_file = "cox-time-varying03.stan")
for(i in 1:10){
  d = dl[[i]]
  
  mod_dat = list(
    N  = nrow(d),
    y = d$marker,
    trt_long = d$trt,
    time = d$times,
    id = d$id,
    n = tapply(d$event_time, d$id, \(i) i[1])|> length(),
    n_time_point = length(times),
    event = tapply(d$censor, d$id, \(i) i[1]),
    trt = tapply(d$trt, d$id, \(i) i[1]),
    r = 0.1,
    c0 = 0.01,
    eps = .Machine$double.eps,
    event_time =tapply(d$event_time, d$id, \(i) i[1]),
    time_point = times
  )
  
  init_fun = \(chain_id){
    set.seed(chain_id)
    list(
      beta = rnorm(2), b0 = rnorm(mod_dat$n), k = rgamma(mod_dat$n,5,5), 
      sigma_b = rgamma(1,5,5), sigma = rgamma(1,5,5),
      alpha_trt = rnorm(1), alpha_mark = rnorm(1),
      dL0 = rgamma(mod_dat$n_time_point-1,5,5)
    )
  }
  
  fit = model$sample(data = mod_dat, chains = 2, parallel_chains = 2, iter_warmup = 6000,
                     iter_sampling = 3000, refresh = 3000, save_cmdstan_config=TRUE, 
                     adapt_delta = 0.9, max_treedepth = 12, init = init_fun)
  
  r = paste0("main-result/s4/d4_",i,".rds")
  fit$save_object(r)
  print(i)
  beep()
}


saveRDS(myresult, file = "main-result/simulation04.rds")

sapply(simulation03, \(d){
  d[,c("beta[2]","alpha_mark", "alpha_trt")]|>
    {\(i) i[,1]*i[,2]+i[,3] }()|>
    {\(i)c(mean(i), hdi(i))}()
})|>
  t()|>
  as_tibble()|>
  `names<-`(c("mean","lower","upper"))|>
  mutate(variable = 1:10, .before = 1)|>
  ggplot( aes(x = reorder(variable, mean), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  # Reference line at RR=1
  geom_hline(yintercept = -1.46, linetype = 2, color = "gray40") +
  coord_flip() +
  # A minimal theme often looks nice for forest plots
  theme_minimal()
  
