library('truncnorm')

mean_log_k <- log(0.0083)
q1_log_k <- log(0.0042)
q3_log_k <- log(0.0166)

mean_beta <- 1/2.16
q1_beta <- 1/1.06
q3_beta <- 1/2.88

sd_log_k <- (q3_log_k - q1_log_k) / 1.35
sd_beta <- (q1_beta - q3_beta) / 1.35

# 50% -> 0.674
z_value <- 1


get_x2 <- function(d, x1, sd){
  x2 <- d * sd + x1
  return(x2)
}

logistic_function <- function(x, k, x0){
  return(10 / (1 + exp(-k * (x - x0))))
}


sd_small <- abs((mean_log_k - get_x2(0.2, mean_log_k, sd_log_k)) / z_value)
sd_medium <- abs((mean_log_k - get_x2(0.5, mean_log_k, sd_log_k)) / z_value)
sd_large <- abs((mean_log_k - get_x2(0.8, mean_log_k, sd_log_k)) / z_value)


x <- seq(-10, 10, length=100)
curve(dnorm(x, mean_log_k, sd_log_k), -9, -1, xlab = expression(log(k)), ylab = 'Density')

curve(dtruncnorm(x, mean = mean_beta, sd = sd_beta, a = 0, b = Inf)
      *logistic_function(x, 20, 0.2), 0, 2, xlab = 'beta', ylab = 'Density')

curve(dnorm(x, 0, sd_small), -3, 3, xlab = expression(s_log(k)), ylab = 'Density', col = 'blue')
curve(dnorm(x, 0, sd_medium), -3, 3, add = TRUE, col = 'red')
curve(dnorm(x, 0, sd_large), -3, 3, add = TRUE, col = 'green')
legend("topright", col = c("blue", "red", "green"), lwd = 2, cex = 0.6, inset = c(0.05, 0), bty = "n",
       legend = c(bquote(sigma == .(round(sd_small, 2))), 
                  bquote(sigma == .(round(sd_medium, 2))),
                  bquote(sigma == .(round(sd_large, 2)))))

# Sampling
hist(rnorm(400, mean = mean_log_k, sd = sd_log_k))
hist(rnorm(400, mean = 0, sd = sd_small))
hist(rnorm(400, mean = 0, sd = sd_medium))
hist(rnorm(400, mean = 0, sd = sd_large))

curve(dnorm(x, mean = mean_log_k, sd = sd_log_k), mean_log_k-4, mean_log_k+4, xlab = 'log(k)', ylab = 'Density')

curve(dnorm(x, mean = mean_beta, sd = sd_beta), mean_beta-2, mean_beta+2, xlab = expression(beta), ylab = 'Density')
curve(logistic_function(x, 1, 3.2), -3, 10, ylim = range(0, 10, 1), xlab = expression(beta), ylab = expression(beta))

x_beta <- rnorm(1000, mean = mean_beta, sd = sd_beta)
hist(x_beta)
hist(logistic_function(x_beta, 1, 3.2), xlab = expression(beta), main = '', xlim = c(0,2.5))

