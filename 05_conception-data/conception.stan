data {
  int<lower=0> N;
  int<lower=0> J;
  vector [N] y;
  array [N] int id;
}

parameters {
  vector [J] gamma;
  real<lower=1e-10> sigma_gam;
  real<lower=1e-10> sigma;
  real mu;
}

// transformed parameters {
//   real<lower=0> ratio = sigma_gam/sigma;
// }

model {
  mu ~ uniform(0,100);
  sigma ~ uniform(0,100);
  sigma_gam ~ uniform(0,100);
  gamma ~ normal(mu, sigma_gam);
  
  y ~ normal(gamma[id], sigma);
}


