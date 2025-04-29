

data {
  int<lower=0> n_sub;
  int<lower=0> n_obs;
  int<lower=0> n_miss;
  vector [n_obs] y_obs;
  vector [n_obs] time_obs;
  array [n_obs] int<lower=0>  ID_obs;
  
  vector [n_miss] time_miss;
  array [n_miss] int<lower=0>  ID_miss;
  
}

transformed data {
}

parameters{
   vector [n_miss] y_miss;
   vector [n_sub] b_raw;
   
   real beta0;
   real beta1;
   real<lower=0> sigma;
   real<lower=0> sigma_b;
}

transformed parameters {
  vector [n_sub] b = b_raw*sigma_b;
  vector [n_obs] mu_obs = beta0 + b[ID_obs] + beta1*time_obs;
  vector [n_miss] mu_miss = beta0 + b[ID_miss] + beta1*time_miss;
}

model {
  
  sigma ~ cauchy(0,1);
  sigma_b ~ cauchy(0,1);
  b_raw ~ normal(0, 1);
  
  beta0 ~ normal(0,100);
  beta1 ~ normal(0,100);
  
  
  y_obs ~ normal(mu_obs, sigma);
  
  y_miss ~ normal(mu_miss, sigma);
  
}













