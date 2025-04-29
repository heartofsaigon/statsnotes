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
  vector[N] y;
  vector [N] x;
  vector [N] w;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector [2] beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector [N] mu;
  for(i in 1:N)
    mu[i] = beta[1] + beta[2]*x[i];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += normal_lpdf(beta|0,1000);
  target += cauchy_lpdf(sigma|0,1);
  for(i in 1:N)
  //  target += w[i]*normal_lpdf(y[i]|mu[i],sigma);
    y[i] ~ normal(mu[i], sigma/sqrt(w[i]));
}

