library("rstan")
library("bayesplot")

fit <- readRDS("out/test3_sd_0_51/model_prior_sd_0_2.rds")

#R-hat plot
traceplot(fit, pars = 'mu_s_log_k')

# Plot with HDI (richtig so?)
posterior <- as.matrix(fit)
mcmc_areas(posterior, pars = c('mu_s_log_k'), prob = 0.95)