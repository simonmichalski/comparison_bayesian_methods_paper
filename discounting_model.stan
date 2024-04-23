
data {
  int<lower=1> n_subj;
  int<lower=1> num_trials;
  int<lower=1> num_conditions
  real<lower=0> ss[num_conditions, n_subj, num_trials];
  real<lower=0> ll[num_conditions, n_subj, num_trials];
  real<lower=0> delay[num_conditions, n_subj, num_trials];
  int<lower=0, upper=1> choice[num_conditions, n_subj, num_trials];
}

parameters {
  real<lower=0> k;
  real<lower=0> k_cond;
  real<lower=0> beta;
}

model {
  for (i in 1:n_subj){
    
    k[i] ~ normal();
    k_cond[i] ~ normal();
    beta ~ ;
    
  }
}
