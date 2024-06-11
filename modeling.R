library('rstan')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


data <- sd_2$data

# Create 3d arrays; condition index 1:2
condition_values <- data$condition
condition_array <- xtabs(condition_values ~ subject + condition + trial, data = data)
ss_array <- xtabs(ss ~ subject + condition + trial, data = data)
ll_array <- xtabs(ll ~ subject + condition + trial, data = data)
delay_array <- xtabs(delay ~ subject + condition + trial, data = data)
choice_array <- xtabs(choice ~ subject + condition + trial, data = data)


stan_data <- list(
  n_subj = 40,
  num_cond = 2,
  num_trials = 128,
  condition = condition_array,
  ss = ss_array,
  ll = ll_array,
  delay = delay_array,
  choice = choice_array
)

model <- stan_model('discounting_model.stan')

fit <- sampling(
  model,
  data = stan_data,
  chains = 2,
  iter = 2000,
  warmup = 1000,
  thin = 1
)


saveRDS(fit, "out/models/test_model_sd_2.rds")
