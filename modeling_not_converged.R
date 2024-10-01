library('foreach')
library('doParallel')
library('parallel')
library('rstan')

not_converged <- readRDS("not_converged.rds")

get_vectors <- function(){
  sds <- vector()
  sample_numbers <- vector()
  prior_sds <- vector()
  
  for (path in not_converged){
    sd_match <- regmatches(path, regexpr("sd_\\d+_\\d+", path))
    sd_values <- unlist(regmatches(sd_match, gregexpr("\\d+", sd_match)))
    sd <- as.numeric(paste(sd_values, collapse = "."))
    sds <- append(sds, sd)
    
    sample_match <- regmatches(path, regexpr("sample_\\d+", path))
    sample_number <- as.numeric(gsub("sample_", "", sample_match))
    sample_numbers <- append(sample_numbers, sample_number)
    
    prior_sd_match <- regmatches(path, regexpr("model_prior_sd_\\d+(_\\d+)?", path))
    prior_sd_values <- unlist(regmatches(prior_sd_match, gregexpr("\\d+", prior_sd_match)))
    prior_sd <- as.numeric(paste(prior_sd_values, collapse = "."))
    prior_sds <- append(prior_sds, prior_sd)
  }
  return(list(sds = sds, sample_numbers = sample_numbers, prior_sds = prior_sds))
}

vectors <- get_vectors()
sds <- vectors$sds
sample_numbers <- vectors$sample_numbers
prior_sds <- vectors$prior_sds


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


get_stan_data <- function(){
  stan_data_list <- list()

  for (i in not_converged){
    data <- readRDS(i)

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

  return(stan_data_list)
}


parallel_modelling <- function(){
  stan_data_list <- get_stan_data()

  num_cores <- detectCores(logical = TRUE)
  cluster <- makeCluster(num_cores - 1)
  registerDoParallel(cluster)

  foreach (i = 1:length(stan_data_list), .packages = 'rstan', .export=ls(envir=globalenv())) %dopar% {

    model <- stan_model(file.path("models", paste0("discounting_model_prior_sd_", gsub("\\.", "_", prior_sds[i]), ".stan")))

    fit <- sampling(
      model,
      data = stan_data_list[[i]],
      chains = 2,
      iter = 2000,
      warmup = 2000,
      thin = 1,
      save_warmup = FALSE
    )

    sd_path <- file.path("out_refit", paste0("sd_", gsub("0.", "0_", sds[i])))
    model_path <- file.path(sd_path, paste0("sample_", sample_numbers[i]), paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[i]), ".rds"))
    saveRDS(fit, model_path)
  }

  stopCluster(cl = cluster)
}


parallel_modelling()
