
cond_sd <- seq(0.1, 1, 0.1)
num_samples <- 300

n_subj <- 40
num_trials <- 128

k_mean <- 0.1
k_sd <- 0.5
beta_mean <- 0.3
beta_sd <- 0.5

ss <- 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)


get_choice <- function(a, k, delay, ss, beta){
  sv <- a/(1+k*delay)
  p_ll <- exp(sv*beta)/(exp(sv*beta)+exp(ss*beta))
  random_number <- runif(1)
  # ss=0; ll=1
  choice <- ifelse(random_number > p_ll, 0, 1)
  return(choice)
}


simulate <- function(n_subj, k_mean, k_sd, sd, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays){
  df <- data.frame()
  for (i in 1:n_subj){
    k <- rnorm(1, mean = k_mean, sd = k_sd)
    k_cond <- rnorm(1, mean = 0, sd = sd)
    beta <- rnorm(1, mean = beta_mean, sd = beta_sd)
    
    for (j in 1:num_trials){
      a <- ss*sample(ss_ratios,1)
      delay <- sample(delays,1)
      
      choice <- get_choice(a, k, delay, ss, beta)
      df <- rbind(df, c(i, 0, j, ss, a, delay, choice))
      
      choice_cond <- get_choice(a, k+k_cond, delay, ss, beta)
      df <- rbind(df, c(i, 1, j, ss, a, delay, choice_cond))
    }
  }
  colnames(df) <- c('subject', 'condition', 'trial', 'ss', 'll', 'delay', 'choice')
  return(df)
}


get_data <- function(cond_sd, num_samples, n_subj, k_mean, k_sd, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays){
  for (i in 1:length(cond_sd)){
    sd_path <- file.path("out/simulation", paste("sd_", gsub("\\.", "_", cond_sd[i]), sep = ""))
    if (!dir.exists(sd_path)) {
      dir.create(sd_path, recursive = TRUE)
    }
    
    for (j in 1:num_samples){
      df <- simulate(n_subj, k_mean, k_sd, cond_sd[i], beta_mean, beta_sd, num_trials, ss, ss_ratios, delays)
      df_path <- file.path(sd_path, paste("sample_", j, ".rds", sep = ""))
      saveRDS(df, file = df_path)
    }
  }
}


#get_data(cond_sd, num_samples, n_subj, k_mean, k_sd, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays)
sd_1 <- simulate(n_subj, k_mean, k_sd, 1, beta_mean, beta_sd, num_trials, ss, ss_ratios, delays)
