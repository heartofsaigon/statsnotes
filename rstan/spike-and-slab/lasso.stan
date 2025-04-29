//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> p;
  vector[N] y;
  matrix [N,p] X;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta0;
  vector [p] beta;
  real<lower=1e-10> sigma;
  real<lower=1e-10> sigma_beta;
  vector<lower=0> [p] tau;
  real<lower=0> lambda;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  
  target += normal_lpdf(beta0|0,100);
  target += cauchy_lpdf(sigma| 0, 1);
  
  target += cauchy_lpdf(sigma_beta|0,5);
  target += exponential_lpdf(lambda|1);
  target += exponential_lpdf(tau|square(lambda)/2);

  
  target += normal_lpdf(beta|0.0, sigma_beta*sqrt(tau));
  // likelihood
  target += normal_lpdf(y| beta0 + X*beta, sigma);  
  
}