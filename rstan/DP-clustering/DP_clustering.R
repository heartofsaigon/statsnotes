
pacman::p_load(tidyverse, cmdstanr, HDInterval, posterior)

d = read_csv(file = "/Users/nam-anhtran/Library/CloudStorage/GoogleDrive-namanhsg4@gmail.com/My Drive/18-Bayes/rstan/DP-clustering/simulated_data.csv")

# 1. Write Stan file
mod <- cmdstan_model("DP_clustering.stan")

# 2. Prepare data list
stan_data <- list(
  N = nrow(d),
  M = 4,
  K = 10,
  X = as.matrix(d[, paste0("G", 1:4)])  # columns G1–G4
)


init_fun = function(chain_id){
  set.seed(chain_id)
  list(
    alpha = rgamma(1,1,1),
    v = rbeta(stan_data$K-1,1,1),
    phi = rnorm(stan_data$M*stan_data$K)|> matrix(ncol = stan_data$M),
    sigma = rgamma(1,1,1)
  )
}




# 3. Sample
fit <- mod$sample(
  data = stan_data,
  chains = 3, parallel_chains = 3, 
  iter_warmup = 8000, iter_sampling = 2000, refresh = 2000,
  max_treedepth = 12, adapt_delta = 0.99,
  init = init_fun
)

# 4. Examine

fit$summary(c("alpha", "v", "phi", "sigma"), rhat)



a<- 
fit$draws(c( "resp"), format = "array")

b<-
lapply(1:50,\(i){
  b = str_detect(names(a), paste0("resp\\[",i,","))
  a[b]|> round(3)
})

sapply(b, sum)


sapply(b, \(i){
  v = i[which.max(i)]
  v
  })|>
  {\(i) i[i>=0.333] }()
  
b[[46]]


ind = fit$draws("indicator", format = "matrix")|>
  {\(i) i[sample(1:nrow(i), 5000, replace = F),] }()

ind[,1]|> table()


fit$draws("pi", format = "matrix")[,10]|> hdi()

sapply(1:50, \(i){
  fit$draws("indicator")[,1,i]|> as.matrix()|> table()
})|>
  

fit$draws("indicator")[,1,50]|> as.matrix()|> table()

 sapply(1:20,\(i) fit$draws("pi")[i,1,]|> as.matrix())|>
   t()|>
   apply(1, \(i) which(i==max(i)) )
 
 
 fit$draws("pi", format = "matrix")[,10]|> hdi()
