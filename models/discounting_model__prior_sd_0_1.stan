
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
  real mu_log_k;
  real<lower=0> sd_log_k;
  real mu_s_log_k;
  real<lower=0> sd_s_log_k;
  real mu_beta;
  real<lower=0> sd_beta;
  
  // Subject-level parameters
  vector[n_subj] log_k;
  vector[n_subj] s_log_k;
  vector<lower=0>[n_subj] beta;
}

model {
  // Priors group-level parameters
  mu_log_k ~ uniform(-20,3);
  sd_log_k ~ uniform(0,2.5);
  mu_s_log_k ~ normal(0,0.1);
  sd_s_log_k ~ uniform(0,2.5);
  mu_beta ~ uniform(0,10);
  sd_beta ~ uniform(0,2.5);
  
  // Priors subject-level parameters
  log_k ~ normal(mu_log_k, sd_log_k);
  s_log_k ~ normal(mu_s_log_k, sd_s_log_k);
  beta ~ normal(mu_beta, sd_beta);
  
  // Likelihood function
  for (i in 1:n_subj){
    
    for (c in 1:num_cond){
      
      for (t in 1:num_trials){
        real sv[i,c,t];
        
        sv[i,c,t] = ll[i,c,t]/(1+exp(log_k[i]+s_log_k[i]*condition[i,c,t])*delay[i,c,t]);
        choice[i,c,t] ~ bernoulli_logit(beta[i]*(sv[i,c,t]-ss[i,c,t]));
      }
    }
  }
}
