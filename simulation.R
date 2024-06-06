library('truncnorm')

cond_sd <- c(0.066, 0.131, 0.197, 0.262)
num_samples <- 300

n_subj <- 40
num_trials <- 128

log_k_mean <- -3.97
log_k_sd <- 0.694

beta_mean <- 0.405
beta_sd <- 0.156

ss <- 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)


get_choice <- function(a, log_k, delay, ss, beta){
  sv <- a/(1+exp(log_k)*delay)
  p_ll <- exp(sv*beta)/(exp(sv*beta)+exp(ss*beta))
  random_number <- runif(1)
  # ss=0; ll=1
  choice <- ifelse(random_number > p_ll, 0, 1)
  return(choice)
}


simulate <- function(n_subj, log_k_mean, log_k_sd, sd, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays){
  data <- data.frame()
  params <- data.frame()
  
  for (i in 1:n_subj){
    log_k <- rnorm(1, mean = log_k_mean, sd = log_k_sd)
    log_k_cond <- rnorm(1, mean = 0, sd = sd)
    beta <- rtruncnorm(1, mean = beta_mean, sd = beta_sd, a = 0, b = Inf)
    
    params <- rbind(params, c(i, log_k, log_k_cond, beta))
    
    for (j in 1:num_trials){
      a <- ss*sample(ss_ratios, 1)
      delay <- sample(delays, 1)
      
      choice <- get_choice(a, log_k, delay, ss, beta)
      data <- rbind(data, c(i, 0, j, ss, a, delay, choice))
      
      choice_cond <- get_choice(a, log_k+log_k_cond, delay, ss, beta)
      data <- rbind(data, c(i, 1, j, ss, a, delay, choice_cond))
    }
  }
  colnames(data) <- c('subject', 'condition', 'trial', 'ss', 'll', 'delay', 'choice')
  colnames(params) <- c('subject', 'log_k', 'log_k_cond', 'beta')
  
  return(list(data = data, params = params))
}


get_data <- function(cond_sd, num_samples, n_subj, log_k_mean, log_k_sd, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays){
  for (i in 1:length(cond_sd)){
    sd_path <- file.path("out", paste0("sd_", gsub("0.", "", cond_sd[i])))
    if (!dir.exists(sd_path)) {
      dir.create(sd_path, recursive = TRUE)
    }
    
    for (j in 1:num_samples){
      data_params <- simulate(n_subj, log_k_mean, log_k_sd, cond_sd[i], beta_mean, beta_sd, num_trials, ss, ss_ratios, delays)
      
      data <- data_params$data
      data_path <- file.path(sd_path, paste0("sample_", j, "_data.rds"))
      saveRDS(data, file = data_path)
      
      params <- data_params$params
      params_path <- file.path(sd_path, paste0("sample_", j, "_params.rds"))
      saveRDS(params, file = params_path)
    }
  }
}


#get_data(cond_sd, num_samples, n_subj, log_k_mean, log_k_sd, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays)

sd_2 <- simulate(n_subj, log_k_mean, log_k_sd, 2, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays)
