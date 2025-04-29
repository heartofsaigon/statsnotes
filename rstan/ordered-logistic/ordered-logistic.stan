
data {
  int<lower=2> num_group;
  int<lower=0> num_data;
  int<lower=1> num_cov;
  array [num_data] int<lower=1,upper=num_group> y;
//  array[num_data] row_vector[num_cov]  X;
  matrix [num_data,num_cov] X;
}

parameters {
  vector[num_cov] beta;
  vector<lower=1e-10,upper=1e10> [num_group-1] thres;
}
 transformed parameters {
   
   // vector [num_group-1] thres_new;
   // thres_new[1] = thres[1];
   // for(i in 2:(num_group-1)){
   //   if(thres[i] - thres[i-1]==0){
   //     thres_new[i] = thres[i]+ 1e-6;
   //   } else{ thres_new[i] = thres[i];}
   // }
   
  // array[num_group-1] real thres_array = to_array_1d(thres);
  // array[num_group-1] real sorted_thres_array = sort(thres_array);
  // vector[num_group-1] sorted_thres_vector = to_vector(sorted_thres_array);
  
   vector [num_group-1] thres_new;
   thres_new = cumulative_sum(thres);
   
   
 }

model {
  
  beta ~ normal(0, 10);
  thres ~ exponential(2);
  // for (n in 1:num_data)
  //   y[n] ~ ordered_logistic(X[n] * beta, thres);
  y ~ ordered_logistic(X*beta,thres_new);
}

