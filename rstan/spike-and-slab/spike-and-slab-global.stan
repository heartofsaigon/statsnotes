
data {
  int<lower=0> N;
  int<lower=0> p;
  vector[N] y;
  matrix [N,p] X;
}

transformed data {
  //real<lower=0> tau = 1e4;
}

parameters {
  real beta0;
  vector [p] beta;
  real<lower=0> sigma_beta;
  real<lower=1e-10> sigma;
//  real <lower=1e-10>  sigma_beta;
//  vector<lower=0,upper=1> [p] decision;
real<lower=0,upper=1> decision;
  
}


model {
  decision ~ beta(1,1);
  sigma ~ cauchy(0,1);
  sigma_beta ~ cauchy(0,1);
  beta0 ~ normal(0,sigma_beta);
  
  for(i in 1:p){
    target+= log_mix(
    //  decision[i], 
    decision,
      normal_lpdf(beta[i]|0.0,sigma_beta),  
      normal_lpdf(beta[i]|0.0,inv(1e3))
    );
  }
  
  target += normal_lpdf(y| beta0 + X*beta, sigma);  
}

generated quantities {
  
  vector [p] inc_ind;
  real log_slab;
  real log_spike;
//  real<lower=0,upper=1> p_inclusion;
  vector [p] p_inclusion;
  
  // for(i in 1:p){
  //   log_slab = log(decision[i]) + normal_lpdf(beta[i] | 0.0, sigma_beta);
  //   log_spike = log(1 - decision[i]) + normal_lpdf(beta[i] | 0.0, inv(1e3));
  //   p_inclusion = exp(log_slab) / (exp(log_slab) + exp(log_spike));
  //   inc_ind[i] = bernoulli_rng(p_inclusion);
  // }
  
    for(i in 1:p){
    log_slab = log(decision) + normal_lpdf(beta[i] | 0.0, sigma_beta);
    log_spike = log(1 - decision) + normal_lpdf(beta[i] | 0.0, inv(1e3));
    p_inclusion[i] = exp(log_slab) / (exp(log_slab) + exp(log_spike));
   inc_ind[i] = bernoulli_rng(p_inclusion[i]);
  }
}



