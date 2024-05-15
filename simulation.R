
n_subj <- 40
num_trials <- 128

k_dist <- c(0.1,0.2)
beta_dist <- c(1,2)

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


simulate <- function(n_subj, k_dist, sd, beta_dist, num_trials, ss, ss_ratios, delays){
  df <- data.frame()
  for (i in 1:n_subj){
    k <- sample(k_dist,1)
    k_cond <- k + rnorm(1, mean = 0, sd = sd)
    beta <- sample(beta_dist,1)
    
    for (j in 1:num_trials){
      a <- ss*sample(ss_ratios,1)
      delay <- sample(delays,1)
      
      choice <- get_choice(a, k, delay, ss, beta)
      df <- rbind(df, c(i, 0, j, ss, a, delay, choice))
      
      choice_cond <- get_choice(a, k_cond, delay, ss, beta)
      df <- rbind(df, c(i, 1, j, ss, a, delay, choice_cond))
    }
  }
  colnames(df) <- c('subject', 'condition', 'trial', 'ss', 'll', 'delay', 'choice')
  return(df)
}


sd_1 <- simulate(n_subj, k_dist, 1, beta_dist, num_trials, ss, ss_ratios, delays)
