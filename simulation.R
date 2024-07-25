
s_log_k_sds <- c(0.2, 0.51, 0.81)

num_samples <- 3
n_subj <- 40
num_trials <- 128

log_k_mean <- -4.79
log_k_sd <- 1.02

beta_mean <- 0.46
beta_sd <- 0.44

ss <- 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)


get_choice <- function(a, log_k, delay, beta){
  sv <- a/(1+exp(log_k)*delay)
  p_ll <- exp(sv*beta)/(exp(sv*beta)+exp(ss*beta))
  random_number <- runif(1)
  # ss=0; ll=1
  choice <- ifelse(random_number > p_ll, 0, 1)
  return(choice)
}


simulate <- function(s_log_k_sd){
  data <- data.frame()
  params <- data.frame()
  
  for (i in 1:n_subj){
    log_k <- rnorm(1, mean = log_k_mean, sd = log_k_sd)
    s_log_k <- rnorm(1, mean = 0, sd = s_log_k_sd)
    beta <- rnorm(1, mean = beta_mean, sd = beta_sd)
    beta <- logistic_function(beta)
    
    params <- rbind(params, c(i, log_k, s_log_k, beta))
    
    for (j in 1:num_trials){
      a <- ss * ss_ratios[((j - 1) %/% length(delays)) + 1]
      delay <- delays[(j - 1) %% length(delays) + 1]
      
      choice <- get_choice(a, log_k, delay, beta)
      data <- rbind(data, c(i, 0, j, ss, a, delay, choice))
      
      choice_cond <- get_choice(a, log_k+s_log_k, delay, beta)
      data <- rbind(data, c(i, 1, j, ss, a, delay, choice_cond))
    }
  }
  colnames(data) <- c('subject', 'condition', 'trial', 'ss', 'll', 'delay', 'choice')
  colnames(params) <- c('subject', 'log_k', 's_log_k', 'beta')
  
  return(list(data = data, params = params))
}


logistic_function <- function(x){
  return(10 / (1 + exp(-1.97 * (x - 1.94))))
}


get_data <- function(){
  for (i in 1:length(s_log_k_sds)){
    
    for (j in 1:num_samples){
      data_params <- simulate(s_log_k_sds[i])
      
      path <- file.path("out", paste0("sd_", gsub("\\.", "_", s_log_k_sds[i])), paste0("sample_", j))
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
      
      data <- data_params$data
      data_path <- file.path(path, "data.rds")
      saveRDS(data, file = data_path)
      
      params <- data_params$params
      params_path <- file.path(path, "params.rds")
      saveRDS(params, file = params_path)
    }
  }
}


get_data()
