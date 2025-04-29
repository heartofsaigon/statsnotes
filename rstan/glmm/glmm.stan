
data {
  int<lower=0> N;
  int<lower=0> J;
  int<lower=0> q;
  array [N] int <lower=1,upper=J> region; 
  array [N] int<lower=0,upper=1> mood;
  matrix [N,q] X;
  
}

parameters {
  vector [q] beta;
  vector [J] b_raw;
  real<lower=0> sigma_b;

}
transformed parameters {
  vector [J] b = b_raw*sigma_b;
}

model {
  
  beta ~  normal(0,100);
  sigma_b ~ cauchy(0,1);
  b_raw ~ normal(0,1);
  
  mood ~ bernoulli_logit( X*beta + b[region]);
}

generated quantities {
  vector [N] log_lik;
  vector [N] eta =  X*beta + b[region];
  log_lik = to_vector(mood) .* eta - log1p_exp(eta);
}






