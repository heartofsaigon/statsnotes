
functions {
  // This function converts the long-format data (subject IDs and measures)
  // into a matrix with one row per subject and K columns per row.
  // If a subject has fewer than K measurements, the last measurement is replicated.
  matrix long_to_matrix(int N, int n, int K, array[] int subject, vector measures) {
    matrix[n, K] measure_mat;  // output matrix: n subjects x K measures
    int current_index = 1;     // pointer to the current measurement in the long vector

    for (s in 1:n) {
      int num_measures = 0;
      // Count the number of measurements for subject s
      while (current_index <= N && subject[current_index] == s) {
        num_measures = num_measures + 1;
        current_index = current_index + 1;
      }
      if (num_measures == 0) {
        // In case no measurement exists for subject s, fill with zeros.
        for (k in 1:K)
          measure_mat[s, k] = 0;
      } else {
        int start_index = current_index - num_measures;  // first index for subject s
        int last_index  = current_index - 1;             // last measurement index for subject s
        for (k in 1:K) {
          if (k <= num_measures)
            measure_mat[s, k] = measures[start_index + k - 1];
          else
            measure_mat[s, k] = measures[last_index];  // replicate last measurement
        }
      }
    }
    return measure_mat;
  }
}

data {
  int<lower=0> N;
  int<lower=0> n;
  int<lower=0> n_time_point;
  vector<lower=0> [n_time_point] time_point;
  array [n] int<lower=0,upper=1> event;
  vector<lower=0> [n] event_time;
  vector[n]  trt;

  vector[N] y;
  vector [N] trt_long;
  vector [N] time;
  array [N] int<lower=1,upper=n> id;
  
  real<lower=0> r;
  real<lower=0> c0;
  real<lower=0> eps;

}//close data

transformed data {
  int<lower=0> Tnum = n_time_point -1;
  array [n,Tnum] int<lower=0,upper=1>  risk; // risk set
  array [n, Tnum] int<lower=0>  dN; // event set
  
  // the following codes do: 
  // - consider the interval starting at time_j, 
  // - if obs died before time_j --> no information --> risk set and dead set iare zero
  // - if obs time greater than time_j, risk set is 1. we just know that patient could die in time_j-time_{j+1} or live longer than this interval
  //    - we now consider if patient died in current interval. if dying, and no censor, dead set is 1, otherwise, zero
  
  for(i in 1:n){for(j in 1:Tnum){ // for each subject, we consider each interval. Tnum equals the number of time point minus 1, as we have n-1 intervals from n nodes
    
    if(event_time[i] - time_point[j] + eps >0){ // if obs greater than current time point, patient in risk and contribute to loglik
    risk[i,j] = 1; // therefore risk =1 
    if(time_point[j+1] - event_time[i] - eps > 0 && event[i]){
      dN[i,j] = 1; // dN is if patient died in interval t_i and t_{i+1}
      } else{dN[i,j] = 0; }
      } else{ // if patient died in the past, there is no information in the current interval (lower bound is time_j)
        risk[i,j] = 0; // therefore risk is zero
        dN[i,j] = 0; // dead set is zero
        }
        }}
//------------------------------------------------------------------------------
 vector [Tnum] dL0_star;
 for(i in 1:Tnum){
    dL0_star[i] = r*(time_point[i+1] - time_point[i])+eps; // r here is gamma_0 on page 50, as H* follows expxonential distn
    }// contribution at each interval based on time length. 
  }

parameters {
  vector [2] beta;
  vector [n] b0;
  vector<lower=0,upper=4> [n] k;
  real<lower=1e-8>  sigma_b;
  real<lower=1e-8> sigma;

  real alpha_trt;
  real alpha_mark;
  vector<lower=1e-8,upper=1e8> [Tnum] dL0; // intensity??
  } 

transformed parameters {
   matrix [n, Tnum] Idt = rep_matrix(eps, n, Tnum);
   vector [N] mu_y = beta[1] + beta[2]*trt_long + b0[id] + k[id].*time;
   matrix [n,Tnum] mu_y_mat = long_to_matrix(N, n, Tnum, id,  mu_y);
   
    for(i in 1:n){for(j in 1:Tnum){
      if(risk[i,j]){Idt[i,j] = exp(alpha_trt*trt[i] + alpha_mark*mu_y_mat[i,j])*dL0[j]+eps;} // this is the model of poisson.
    // risk[i,j] tell us if that patient is in risk, if so, contribution, which is dL, is considered
    }}
    
   real b0_bar = mean(b0); 
   real k_bar = mean(k);
//  {
// matrix[n, Tnum] trt_mat =(trt) * rep_vector(1.0, Tnum)';
// matrix[n, Tnum] dL0_mat = rep_vector(1.0, n) * dL0';
// Idt = rep_matrix(eps, n, Tnum)
//     + to_matrix(risk) .* ( exp(alpha_trt * trt_mat + alpha_mark * marker) .* dL0_mat );
// }

}

model {
  alpha_trt ~ normal(0,10);
  alpha_mark ~ normal(0,3);
  target += gamma_lpdf(dL0|dL0_star*c0, c0);
 // for(i in 1:n){for(j in 1:Tnum){target += poisson_lpmf(dN[i,j]|Idt[i,j]);}}
  beta ~ normal(0,10);
  sigma_b ~ normal(0, 5);
  b0 ~ normal(0,10);
  k ~ uniform(0,4);
  sigma ~ normal(0,5);
  
  // likelihood 
  y ~ normal(mu_y,sigma);
  for(i in 1:Tnum){target += poisson_lpmf(dN[,i]|Idt[,i]);}
  
}

generated quantities {
  // vector [n*Tnum] log_lik;
  // int k = 1;
  // for(i in 1:n){for(j in 1:Tnum){
  //   log_lik[k] = poisson_lpmf(dN[i,j]|Idt[i,j]);
  //   k+=1;
  // }}
  
  // real ll1=0;
  // real ll2 = 0;
  // for(i in 1:n){for(j in 1:Tnum){ll1 += poisson_log_lpmf(dN[i,j]|log_rate[i,j]);}}
  // ll2 += poisson_log_lpmf(to_array_1d(dN)|to_array_1d(log_rate));
  
}
