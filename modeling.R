library('rstan')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

num_cond = 2


fit <- stan(
  file='modeling.R',
  data=
)