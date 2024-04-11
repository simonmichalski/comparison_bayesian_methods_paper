
num_trials = 128
n = 40
k = .5
beta = .3

ss = 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)


compute_sv <- function(ss, ss_ratios, k, delays){
  a = ss*sample(ss_ratios,1)
  k = k
  D = sample(delays,1)
  sv = a/(1+k*D)
  return(list(a,k,D,sv))
}


get_choice <- function(sv, ss, beta){
  p_ll <- exp(sv*beta)/(exp(sv*beta)+exp(ss*beta))
  random_number <- runif(1)
  choice <- ifelse(random_number > p_ll, 'ss', 'll')
  return(list(beta,choice))
}


simulate <- function(n, num_trials, ss, ss_ratios, k, delays, beta){
  df <- data.frame()
  for (i in 1:n){
    for (j in 1:num_trials){
      a = ss*sample(ss_ratios,1)
      k = k
      D = sample(delays,1)
      sv = a/(1+k*D)
      beta = beta
      p_ll <- exp(sv*beta)/(exp(sv*beta)+exp(ss*beta))
      random_number <- runif(1)
      choice <- ifelse(random_number > p_ll, 'ss', 'll')
      df <- rbind(df, c(i, j, ss, a, k, D, sv, beta, p_ll, random_number, choice))
    }
  }
  colnames(df) <- c('participant', 'trial_number', 'ss', 'll', 'k', 'delay', 
                    'sv', 'beta', 'p_ll', 'random_number', 'choice')
  return(df)
}


test <- simulate(n, num_trials, ss, ss_ratios, k, delays, beta)


# Sample from normal distribution
rnorm(1, mean = 0, sd = 1)


