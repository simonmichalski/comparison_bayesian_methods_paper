library("rstan")
library("bayesplot")


fit <- readRDS("out/sd_0_2/sample_1/model_prior_sd_0_1.rds")
sim_params <- readRDS("out/sd_0_2/sample_1/params.rds")

summary_list <- summary(fit)
summary_all_chains <- summary_list$summary
posterior <- as.matrix(fit)


#R-hat
rhat_mu_s_log_k <- summary_list$summary['mu_s_log_k', 'Rhat']
traceplot(fit, pars = 'mu_s_log_k')


# dBF


#integrate(posterior, lower = -Inf, upper = 0)

# Savage-Dickey BF


# P(effect > 0)
list_of_samples <- extract(fit)
mu_s_log_k_samples <- list_of_samples$mu_s_log_k
p_effect <- sum(mu_s_log_k_samples > 0) / length(mu_s_log_k_samples)


# HDI
mcmc_areas(posterior, pars = c('mu_s_log_k'), prob = 0.95)




