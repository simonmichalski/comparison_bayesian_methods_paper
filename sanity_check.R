library('truncnorm')
library("rstan")


fit <- readRDS("out/sd_0_51/sample_1/model_prior_sd_0_1.rds")
sim_params <- readRDS("out/sd_0_51/sample_1/params.rds")

summary_list <- summary(fit)
summary_all_chains <- summary_list$summary

correlate_model_simulation <- function(summary_all_chains, sim_params){
  log_k_model <- vector()
  s_log_k_model <- vector()
  beta_model <- vector()
  
  for (i in 1:40){
    log_k_model[i] <- summary_all_chains[paste0("log_k[",i,"]"), "mean"]
    s_log_k_model[i] <- summary_all_chains[paste0("s_log_k[",i,"]"), "mean"]
    beta_model[i] <- summary_all_chains[paste0("beta[",i,"]"), "mean"]
  }
  
  log_k_sim <- sim_params$log_k
  s_log_k_sim <- sim_params$s_log_k
  beta_sim <- sim_params$beta
  
  print(paste("r_log(k):", cor(log_k_model, log_k_sim)))
  print(paste("r_s_log(k):", cor(s_log_k_model, s_log_k_sim)))
  print(paste("r_beta:", cor(beta_model, beta_sim)))
}

correlate_model_simulation(summary_all_chains, sim_params)


# Check log(k) and beta distributions
x <- seq(-10, 4, length=100)
curve(dnorm(x, -4.79, 1.02), -9, -1, xlab = expression(log(k)), ylab = 'Density')

curve(dnorm(x, 0, sd_small), -5, 5, xlab = expression(s_log(k)), ylab = 'Density', col = 'blue')
curve(dnorm(x, 0, sd_medium), -5, 5, add = TRUE, col = 'red')
curve(dnorm(x, 0, sd_large), -5, 5, add = TRUE, col = 'green')
legend("topright", legend = c(paste("sd = ", round(sd_small, 2)), 
                              paste("sd = ", round(sd_medium, 2)),
                              paste("sd = ", round(sd_large, 2))),
       col = c("blue", "red", "green"), lwd = 2, cex = 0.7)

logistic_function <- function(x, k, x0){
  return(1 / (1 + exp(-k * (x - x0))))
}

x <- seq(0, 1, length = 100)

curve(logistic_function(x, 5, 0.4))

curve(dtruncnorm(x, mean = .46, sd = 0.44, a = 0, b = Inf)*logistic_function(x, 20, 0.2), 0, 2)

curve(dtruncnorm(x, mean = .46, sd = 0.44, a = 0, b = Inf), 0, 2)
curve(logistic_function(x, 1000, 0.01))


# Simulate single subjects and print choice stats
get_subj_stats <- function(data_params){
  data <- data_params$data
  params <- data_params$params
  num_ll_choices <- sum(data$choice)
  print(paste("% ll choices = ", num_ll_choices/nrow(data)))
  
  log_k <- params$log_k
  s_log_k <- params$s_log_k
  data$sv <- data$ll/(1+exp(log_k+s_log_k*data$condition)*data$delay)
  data$consistency <- ifelse(data$sv > data$ss & data$choice == 1, 1, 0)
  num_consistent_trials <- sum(data$consistency)
  print(paste("% consistent choices = ", num_consistent_trials/nrow(data)))
}

ss <- 20
ss_ratios <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
               1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delays <- c(1, 3, 5, 8, 14, 30, 60, 120)
num_trials <- 128

log_k <- -1
beta <- 5

subj <- simulate(1)
get_subj_stats(subj)

