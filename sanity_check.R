library('truncnorm')

# Check log(k) and beta distributions
x <- seq(-10, 4, length=100)
curve(dnorm(x, -4.79, 2.75), -14, 5)

logistic_function <- function(x, k, x0){
  return(1 / (1 + exp(-k * (x - x0))))
}

x <- seq(0, 1, length = 100)
curve(logistic_function(x, 2, 9))

curve(dtruncnorm(x, mean = .46, sd = 1.19, a = 0, b = Inf)*logistic_function(x, 1000, 0.01), 0, 4)

curve(dtruncnorm(x, mean = .46, sd = 1.19, a = 0, b = Inf), 0, 4)
curve(logistic_function(x, 1000, 0.01))


# Simulate single subjects and print choice stats
get_subj_stats <- function(data_params){
  data <- data_params$data
  params <- data_params$params
  num_ll_choices <- sum(data$choice)
  print(paste("% ll choices = ", num_ll_choices/nrow(data)))
  
  log_k <- params$log_k
  log_k_cond <- params$log_k_cond
  data$sv <- data$ll/(1+exp(log_k+log_k_cond*data$condition)*data$delay)
  data$consistency <- ifelse(data$sv > data$ss & data$choice == 1, 1, 0)
  num_consistent_trials <- sum(data$consistency)
  print(paste("% consistent choices = ", num_consistent_trials/nrow(data)))
}

ss <- 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)
num_trials <- 128

log_k <- -6
beta <- 5

subj <- simulate(1, log_k, 0, 0.1, beta, 0, num_trials, ss, ss_ratios, delays)
get_subj_stats(subj)

