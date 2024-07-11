
median_beta <- 1/2.16
q1_beta <- 1/1.06
q3_beta <- 1/2.88

mean_beta <- 1/2.16
sd_beta <- (q1_beta - q3_beta) / 1.35

x0_params <- seq(1.9,2,0.01)
k_params <- seq(1.9,2,0.01)

logistic_function <- function(x, k, x0){
  return(10 / (1 + exp(-k * (x - x0))))
}

threshold <- 0.054

for (i in 1:length(x0_params)){
  print(paste0(i, "/", length(x0_params)))
  for (j in 1:length(k_params)){
    x_beta <- rnorm(1000000, mean = mean_beta, sd = sd_beta)
    beta_transformed <- logistic_function(x_beta, k_params[j], x0_params[i])
    if ((abs(median(beta_transformed) - median_beta) < threshold) &
        (abs(as.numeric(quantile(beta_transformed)[2]) - q3_beta) < threshold) &
        (abs(as.numeric(quantile(beta_transformed)[4]) - q1_beta) < threshold)){
        print(x0_params[i])
        print(k_params[j])
    }
  }
}
