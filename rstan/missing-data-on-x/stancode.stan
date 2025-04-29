
// file: joint_impute_regression.stan
data {
  int<lower=1> N;                      // 50
  int<lower=1> P;                      // 5
  vector[N]    y;                      // outcome
  matrix[N,P]  X_obs;                  // observed covariates with NA slots
  int<lower=0> M;                      // number of missing entries
  array [M] int <lower=1,upper=N>  miss_row;    // row indices of missing
  array [M] int <lower=1,upper=P>  miss_col;    // col indices of missing
}
parameters {
  real              beta0;
  vector[P]         beta;
  real<lower=1e-10>     sigma;

  // joint prior on X rows
  vector[P]         mu_x;
  vector<lower=1e-10>[P] tau_x;
  cholesky_factor_corr[P] L_corr;

  // the missing entries
  vector[M]         x_miss;
}
transformed parameters {
  matrix[N,P] X;
  X = X_obs;
  for (m in 1:M)
    X[ miss_row[m], miss_col[m] ] = x_miss[m];

  // build Cholesky factor of cov(X) = diag(tau_x) * Corr * diag(tau_x)
  matrix[P,P] L_Sigma = diag_pre_multiply(tau_x, L_corr);
}
model {
  // Priors for regression
  beta0   ~ normal(0, 5);
  beta    ~ normal(0, 2);
  sigma   ~ cauchy(0, 2.5);

  // Priors for X joint model
  mu_x    ~ normal(0, 3);
  tau_x   ~ cauchy(0, 2.5);
  L_corr  ~ lkj_corr_cholesky(2);

  // Imputation + joint prior on each row of X
  for (i in 1:N)
    X[i] ~ multi_normal_cholesky(mu_x, L_Sigma);

  // Likelihood
  y ~ normal(beta0 + X * beta, sigma);
}

generated quantities {
  corr_matrix[P] Omega = multiply_lower_tri_self_transpose(L_corr);
}















