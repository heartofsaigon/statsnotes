// dp_mixture_4gene.stan
data {
  int<lower=1> N;          // number of samples (50)
  int<lower=1> M;          // number of genes (4)
  int<lower=1> K;          // truncation level (e.g. 10)
  matrix[N, M] X;          // gene-expression: each column is one sample
}


parameters {
  real<lower=0, upper=1e10> alpha;                 // DP concentration
  vector<lower=0,upper=1>[K-1] v;      // stick-breaking betas
  matrix[K, M] phi;                   // K potential cluster centroids
  real<lower=1e-8> sigma;                 // observation noise
}


transformed parameters {
  simplex[K] pi;                       // mixture weights
  {
    vector[K] rem;
    rem[1] = 1;
    for (k in 1:(K-1)) {
      pi[k]    = v[k] * rem[k];
      rem[k+1] = rem[k] * (1 - v[k]);
    }
    pi[K] = rem[K];
  }
}


model {
  // Priors
  alpha ~ gamma(1, 1);
  v     ~ beta(1, alpha);
  for (k in 1:K){
    phi[k,] ~ normal(0, 1);            // base G0 = MVN(0, I)
  }
  sigma ~ cauchy(0, 2);

  // Marginalized mixture likelihood
  for (n in 1:N) {
    vector[K] lps;
    for (k in 1:K){
      lps[k] = log(pi[k])+ normal_lpdf(X[n,] | phi[k,], sigma);
      }
    target += log_sum_exp(lps);
  }
}


generated quantities {
  // posterior “responsibilities” resp[n] = P(cluster = k | x[n], parameters)
  array[N] simplex[K] resp;
  array[N] int<lower=1> indicator;

  for (n in 1:N) {
    vector[K] log_p;         // un-normalized log posterior for each component
    for (k in 1:K) {
      log_p[k] = log(pi[k])+ normal_lpdf( X[n,] | phi[k,], sigma);
    }
    // softmax(log_p) = exp(log_p) / sum(exp(log_p))
    resp[n] = softmax(log_p);
    indicator[n] = categorical_rng(resp[n]);
  }
}


