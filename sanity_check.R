
# Check log(k) and beta distributions
x <- seq(-10, 4, length=100)
curve(dnorm(x, -3.97, .694), -6, -2)
curve(dnorm(x, .405, .156), 0, 1)


# Simulate single subjects and print choice stats
get_subj_stats <- function(data_params){
  data <- data_params$data
  params <- data_params$params
  num_ll_choices <- sum(data$choice)
  print(paste("% ll choices = ", num_ll_choices/nrow(data)))
  
  log_k <- params$log_k
  k_cond <- params$k_cond
  data$sv <- data$ll/(1+exp(log_k+k_cond*data$condition)*data$delay)
  data$consistency <- ifelse(data$sv > data$ss & data$choice == 1, 1, 0)
  num_consistent_trials <- sum(data$consistency)
  print(paste("% consistent choices = ", num_consistent_trials/nrow(data)))
}


subj <- simulate(1, -3, 0, 0.1, 0.8, 0, num_trials, ss, ss_ratios, delays)
get_subj_stats(subj)

