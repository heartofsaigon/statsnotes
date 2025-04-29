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

functions {
  real MyFunc(real y, real lam){
    real k;
    k = log(lam)- lam*y;
    return k;
  }
}

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> lambda;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += gamma_lpdf(lambda|1.0,1.0);
  for(i in 1:N) 
    target += MyFunc(y[i], lambda);
}

generated quantities {
  vector [N] m;
  for(i in 1:N)
    m[i] = MyFunc(y[i], lambda);
}

