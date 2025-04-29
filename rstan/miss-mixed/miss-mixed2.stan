
functions {
  // Helper function to check if a value is in an integer array
  int is_in(int key, array [] int arr) {
    for (i in 1:num_elements(arr))
      if (arr[i] == key)
        return 1;
    return 0;
  }
  
  // Helper function to get the index position of a key in an integer array.
  int get_index(int key, array[] int arr) {
    for (i in 1:num_elements(arr))
      if (arr[i] == key)
        return i;
    // Return 0 if not found (this should never happen if key is in arr)
    return 0;
  }
}


data {
  int<lower=0> N;
  int<lower=0> n_sub;
  int<lower=0> n_obs;
  vector [n_obs] y_obs;
  array [N] int<lower=0> ID;
  vector [N] time;
  array [n_obs] int <lower=0> obs_ind;
  array [N-n_obs] int <lower=0> miss_ind;
  
}

parameters {
   vector [N - n_obs] y_miss;
   vector [n_sub] b;

   real beta0;
   real beta1;
   real<lower=0> sigma;
   real<lower=0> sigma_b;
}

transformed parameters {

vector [N] y;

y[obs_ind] = y_obs;
y[miss_ind] = y_miss;

// {
// int miss_counter = 1;
// for(i in 1:N){
//   
//   if(is_in(i, obs_ind)){
//     y[i] = y_obs[get_index(i,obs_ind)];
//   } else {
//     y[i] = y_miss[miss_counter];
//     miss_counter += 1;
//   }
// }
// }



}


model {
  sigma ~ cauchy(0,1);
  sigma_b ~ cauchy(0,1);
  b ~ normal(0, sigma_b);
  
  beta0 ~ normal(0,100);
  beta1 ~ normal(0,100);
  
  // for( i in 1: N){
  //   y[i] ~ normal(beta0 + b[ID[i]] + beta1*time[i] , sigma);
  // }
  
  y ~ normal(beta0 + b[ID] + beta1*time, sigma);
}









