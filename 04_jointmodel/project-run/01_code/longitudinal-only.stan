
data {
  int<lower=0> N;
  int<lower=0> n;
  vector[N] y;
  vector [N] trt;
  vector [N] time;
  array [N] int<lower=1,upper=n> id;
}

parameters {
  vector [2] beta;
  vector [n] b0;
  vector <lower=1e-6,upper=1e6> [n] b1;
  //vector [n] b0;
  //vector [n] b1;
  //cholesky_factor_corr[2] L;  
  //matrix[2,n] z;
  real<lower=1e-8>  sigma_b;
  real<lower=1e-8> sigma;
}

transformed parameters {
  vector [2] b_mean;
  b_mean[1] = mean(b0);
  b_mean[2] = mean(b1);
}

model {
  beta ~ normal(0,10);
  sigma_b ~ normal(0, 5);
  b0 ~ normal(0,10);
  b1 ~ gamma(1, 1);
  sigma ~ normal(0,5);
  
  vector [N] mu_y = beta[1] + beta[2]*trt + b0[id] + b1[id].*time;
  y ~ normal(mu_y,sigma);
  
  
  
}






