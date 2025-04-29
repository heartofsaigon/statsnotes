
data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0,upper=1> y[N];
  vector[N] x;
  vector[N] time;
  int<lower=0> id[N];
  
  
}


parameters {
  real beta[3];
  vector [K] b;
  real<lower=0,upper=1> se;
  real<lower=0,upper=1> sp;
  real<lower=0> sigma;
  real<lower=0> sigma_b;

}

transformed parameters {
  vector [N] p_z;
  real<lower=0,upper=1> sp2 = 1-sp;
  for(i in 1:N)
    p_z[i] = inv_logit(beta[1] + beta[2]*x[i] + beta[3]*time[i] + b[id[i]]);
}


model {
  
  target += normal_lpdf(beta|0,sigma);
  target += beta_lpdf(se|5,5);
  target += beta_lpdf(sp| 5,5);
  target+= normal_lpdf(b|0, sigma_b);
  target += cauchy_lpdf(sigma_b|0,1);
  target += cauchy_lpdf(sigma|0,1);

  for(i in 1:N)
     //target += log_mix(p_z[i], bernoulli_lpmf(y[i]|se), bernoulli_lpmf(y[i]|sp2));
    target += bernoulli_lpmf(y[i]|p_z[i]);
  
  
}

