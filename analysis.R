library("bayesplot")
library("rstan")


#R-hat
rhat_mu_S_log_k <- summary_list$summary['mu_s_log_k', 'Rhat']
traceplot(fit, pars = 'mu_s_log_k')


# Plot
posterior <- as.matrix(fit)
mcmc_areas(posterior, pars = c('s_log_k[1]', 's_log_k[2]', 's_log_k[3]'), prob = 0.95)
mcmc_areas(posterior, pars = c('mu_log_k'), prob = 0.95)
mcmc_areas(posterior, pars = c('mu_s_log_k'), prob = 0.95)
mcmc_areas(posterior, pars = c('mu_beta'), prob = 0.95)




