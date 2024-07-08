library("bayesplot")
library("rstan")

fit <- readRDS("out/test_sd_0_51/model_prior_sd_0_2.rds")
sim_params <- readRDS("out/test_sd_0_51/params.rds")


summary_list <- summary(fit)
summary_all_chains <- summary_list$summary


correlate_model_simulation <- function(summary_all_chains, sim_params){
  log_k_model <- vector()
  s_log_k_model <- vector()
  beta_model <- vector()
  
  for (i in 1:40){
    log_k_model[i] <- summary_all_chains[paste0("log_k[",i,"]"), "mean"]
    s_log_k_model[i] <- summary_all_chains[paste0("s_log_k[",i,"]"), "mean"]
    beta_model[i] <- summary_all_chains[paste0("beta[",i,"]"), "mean"]
  }
  
  log_k_sim <- sim_params$log_k
  s_log_k_sim <- sim_params$s_log_k
  beta_sim <- sim_params$beta
  
  print(paste("r_log(k):", cor(log_k_model, log_k_sim)))
  print(paste("r_log(k)_cond:", cor(s_log_k_model, s_log_k_sim)))
  print(paste("r_beta:", cor(beta_model, beta_sim)))
}


correlate_model_simulation(summary_all_chains, sim_params)


# Plot
posterior <- as.matrix(fit)
mcmc_areas(posterior, pars = c('log_k_cond[1]', 'log_k_cond[2]', 'log_k_cond[3]'), prob = 0.95)
mcmc_areas(posterior, pars = c('mu_log_k'), prob = 0.95)
mcmc_areas(posterior, pars = c('mu_log_k_cond'), prob = 0.95)
mcmc_areas(posterior, pars = c('mu_beta'), prob = 0.95)




