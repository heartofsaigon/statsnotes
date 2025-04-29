data {
  int<lower=0> N;
  int<lower=0> p;
  vector[N] y;
  matrix [N,p] X;
}


parameters {
  real beta0;
  vector [p] beta;
  real<lower=0> sigma;
  vector<lower=0,upper=1e6> [p] sigma_beta;
  real<lower=0,upper=1e6> tau;
}

transformed parameters {
  vector[p] beta_new = beta.*sigma_beta*tau;
  vector [N] mu_y = beta0 + X*beta_new;
}

model {
  
  target += cauchy_lpdf(sigma| 0, 5);
  target += normal_lpdf(sigma_beta|0,0.5);
  target += normal_lpdf(tau|0,0.5);
  target += normal_lpdf(beta0|0,10);
 // target += normal_lpdf(beta|sigma_beta.*tau)
 target += normal_lpdf(beta|0,1);
  
target += normal_lpdf(y|mu_y , sigma);  

//print("tau = ", tau, ", sigma_beta = ", sigma_beta, ", beta_new = ", beta_new, ", mu_y[1] = ", mu_y[1]);

}

