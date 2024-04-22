
num_trials = 128
n = 40

k_dist = c(1,2)
beta_dist = c(1,2)

ss = 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)


get_choice <- function(a, k, D, ss, beta){
  sv = a/(1+k*D)
  p_ll <- exp(sv*beta)/(exp(sv*beta)+exp(ss*beta))
  random_number <- runif(1)
  choice <- ifelse(random_number > p_ll, 'ss', 'll')
  return(choice)
}


simulate <- function(n, k_dist, sd, beta_dist, num_trials, ss, ss_ratios, delays){
  df <- data.frame()
  for (i in 1:n){
    k = sample(k_dist,1)
    k_cond = k + rnorm(1, mean = 0, sd = sd)
    beta = sample(beta_dist,1)
    
    for (j in 1:num_trials){
      a = ss*sample(ss_ratios,1)
      D = sample(delays,1)
      
      choice <- get_choice(a, k, D, ss, beta)
      df <- rbind(df, c(i, 0, j, k, beta, choice))
      
      choice_cond <- get_choice(a, k_cond, D, ss, beta)
      df <- rbind(df, c(i, 1, j, k_cond, beta, choice_cond))
    }
  }
  colnames(df) <- c('subject', 'condition', 'trial', 'k', 'beta', 'choice')
  return(df)
}


sd_1 <- simulate(n, k, 1, beta, num_trials, ss, ss_ratios, delays)


