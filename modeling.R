library('rstan')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


data <- sd_1

# Create 3d arrays; condition index 1:2
condition_values <- data$condition
condition_array <- xtabs(condition_values ~ subject + condition + trial, data = data)
ss_array <- xtabs(ss ~ subject + condition + trial, data = data)
ll_array <- xtabs(ll ~ subject + condition + trial, data = data)
delay_array <- xtabs(delay ~ subject + condition + trial, data = data)
choice_array <- xtabs(choice ~ subject + condition + trial, data = data)


stan_data <- list(
  n_subj <- 40,
  num_cond <- 2,
  num_trials <- 128,
  condition <- condition_array,
  ss <- ss_array,
  ll <- ll_array,
  delay <- delay_array,
  choice <- choice_array
)


fit <- stan(
  file = 'discounting_model.stan',
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = floor(iter/2),
  thin = 1,
  init = 'random'
)
