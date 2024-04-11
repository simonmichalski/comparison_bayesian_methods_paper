
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
  df <- data.frame(participant=integer(),
                   trial_number=integer(),
                   ss=integer(),
                   ll=integer(),
                   k=integer(),
                   delay=integer(),
                   beta=integer(),
                   choice=character())
  for (i in n){
    for (j in num_trials){
      a, k, D, sv <- compute_sv(ss, ss_ratios, k, delays)
      beta, choice <- get_choice(sv, ss, beta)
      df <- rbind(df, list(i, j, ss, sv, k))
    }
  }
  return(df)
}

