
data {
  int<lower=1> n_subj;
  int<lower=1> num_cond
  int<lower=1> num_trials;
  int<lower=0, upper=1> condition[n_subj, num_cond, num_trials];
  real<lower=0> ss[n_subj, num_cond, num_trials];
  real<lower=0> ll[n_subj, num_cond, num_trials];
  real<lower=0> delay[n_subj, num_cond, num_trials];
  int<lower=0, upper=1> choice[n_subj, num_cond, num_trials];
}

parameters {
  // Hyperparameters
  real mu_k;
  real sd_k;
  real mu_k_cond;
  real sd_k_cond;
  real mu_beta;
  real sd_beta;
  
  // Subject-level parameters
  vector[n_subj] k;
  vector[n_subj] k_cond;
  vector[n_subj] beta;
}

model {
  
  // Priors
  k ~ normal(mu_k, sd_k);
  k_cond ~ normal(mu_k_cond, sd_k_cond);
  beta ~ normal(mu_beta, sd_beta);
  
  // Likelihood
  for (i in 1:n_subj){
    
    for (c in 1:num_cond){
      
      for (t in 1:num_trials){
        
        sv[i,c,t] = ll[i,c,t]/(1+(k[i]+k_cond[i]*condition[i,c,t])*delay[i,c,t])
      }
    }
  }
}





