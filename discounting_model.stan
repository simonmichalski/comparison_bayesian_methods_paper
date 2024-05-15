
data {
  int<lower=1> n_subj;
  int<lower=1> num_cond;
  int<lower=1> num_trials;
  int<lower=0, upper=1> condition[n_subj, num_cond, num_trials];
  real<lower=0> ss[n_subj, num_cond, num_trials];
  real<lower=0> ll[n_subj, num_cond, num_trials];
  real<lower=0> delay[n_subj, num_cond, num_trials];
  int<lower=0, upper=1> choice[n_subj, num_cond, num_trials];
}

parameters {
  // Hyperparameters (group-level)
  real mu_k;
  real sd_k;
  real mu_beta;
  real sd_beta;
  real mu_k_cond;
  real sd_k_cond;
  real mu_beta_cond;
  real sd_beta_cond;
  
  // Subject-level parameters
  vector[n_subj] k;
  vector[n_subj] k_cond;
  vector[n_subj] beta;
}

model {
  // Priors group-level parameters (-> erotic cue exposure paper)
  mu_k ~ uniform(-20,3);
  sd_k ~ cauchy(0,2.5);
  mu_beta ~ uniform(0, 10);
  sd_beta ~ cauchy(0,2.5);
  mu_k_cond ~ normal(0, 2);
  sd_k_cond ~ cauchy(0,2.5);
  mu_beta_cond ~ normal(0,2);
  sd_beta_cond ~ cauchy(0,2.5);
  
  // Priors subject-level parameters
  k ~ normal(mu_k, sd_k);
  beta ~ normal(mu_beta, sd_beta);
  k_cond ~ normal(mu_k_cond, sd_k_cond);
  beta_cond ~ normal(mu_beta_cond, sd_beta_cond);
  
  // Likelihood function
  for (i in 1:n_subj){
    
    for (c in 1:num_cond){
      real sv[i,c,t];
      
      for (t in 1:num_trials){
        sv[i,c,t] = ll[i,c,t]/(1+(k[i]+k_cond[i]*condition[i,c,t])*delay[i,c,t])
        choice[i,c,t] ~ exp(sv[i,c,t]*(beta[i]+beta_cond[i]*condition[i,c,t])/(exp[i,c,t](sv*(beta[i]+beta_cond[i]*condition[i,c,t])+exp(ss[i,c,t]*(beta[i]+beta_cond[i]*condition[i,c,t])))
      }
    }
  }
}





