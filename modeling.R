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


get_stan_data <- function(){
  stan_data_list <- list()
  
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
      
      stan_data_list <- append(stan_data_list, list(stan_data))
    }
  }
  
  return(stan_data_list)
}


parallel_modelling <- function(){
  stan_data_list <- get_stan_data()
  
  num_cores <- detectCores(logical = TRUE)
  cluster <- makeCluster(num_cores - 1)
  registerDoParallel(cluster)
  
  foreach (i = 1:(length(stan_data_list)*length(prior_sds)), .packages = 'rstan', .export=ls(envir=globalenv())) %dopar% {
    prior_index <- (i - 1) %% length(prior_sds) + 1
    model <- stan_model(file.path("models", paste0("discounting_model_prior_sd_", gsub("\\.", "_", prior_sds[prior_index]), ".stan")))
    
    stan_data_index <- (i - 1) %/% length(prior_sds) + 1
    
    fit <- sampling(
      model,
      data = stan_data_list[stan_data_index],
      chains = 2,
      iter = 2000,
      warmup = 1000,
      thin = 1,
      save_warmup = FALSE
    )
    
    s_log_k_sd_index <- rep(rep(1:length(s_log_k_sds), each = num_samples * length(prior_sds)), length.out = length(stan_data_list)*length(prior_sds))
    sample_numbers <- rep(rep(1:num_samples), each = length(prior_sds), length.out = length(stan_data_list)*length(prior_sds))
    
    sd_path <- file.path("out", paste0("sd_", gsub("0.", "0_", s_log_k_sds[s_log_k_sd_index[i]])))
    model_path <- file.path(sd_path, paste0("sample_", sample_numbers[i]), paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[prior_index]), ".rds"))
    saveRDS(fit, model_path)
  }
  
  stopCluster(cl = cluster)
}


parallel_modelling()
