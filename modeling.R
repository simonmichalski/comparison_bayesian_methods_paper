library('foreach')
library('doParallel')
library('parallel')
library('rstan')

s_log_k_sds <- c(0.2, 0.51, 0.81)
num_samples <- 3
#prior_sds <- c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)
prior_sds <- c(0.1, 0.5, 1.5)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

for (i in 1:length(s_log_k_sds)){
  sd_path <- file.path("out", paste0("sd_", gsub("0.", "0_", s_log_k_sds[i])))
  
  for (j in 1:num_samples){
    data_path <- file.path(sd_path, paste0("sample_", j), "data.rds")
    data <- readRDS(data_path)
    
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
    
    num_cores <- detectCores(logical = TRUE)
    cluster <- makeCluster(num_cores - 1)
    registerDoParallel(cluster)
    
    foreach (k = 1:length(prior_sds), .packages = 'rstan') %dopar% {
      model <- stan_model(file.path("models", paste0("discounting_model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".stan")))
      
      fit <- sampling(
        model,
        data = stan_data,
        chains = 1,
        iter = 10,
        warmup = 5,
        thin = 1,
        save_warmup = FALSE
      )
      
      model_path <- file.path(sd_path, paste0("sample_", j), paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".rds"))
      saveRDS(fit, model_path)
    }
    
    stopCluster(cl = cluster)
  }
}
