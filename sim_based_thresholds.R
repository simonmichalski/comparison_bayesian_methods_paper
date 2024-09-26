library("rstan")
library("bayestestR")

s_log_k_sds <- c(0.2, 0.51, 0.81)
num_samples <- 100
prior_sds <- c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)

df_results <- readRDS("final_results.rds")


get_simulation_based_thresholds <- function(results, n_tests){
  sim_based_thresholds <- data.frame()
  
  for (i in 1:n_tests){
    savage_dickey_bf <- quantile(results$savage_dickey_bf, probs = 1-(0.05/n_tests))[[1]]
    
    directional_bf_upper <- quantile(results$directional_bf, probs = 1-(0.025/n_tests))[[1]]
    directional_bf_lower <- quantile(results$directional_bf, probs = 0.025/n_tests)[[1]]
    
    hdi <- tryCatch({uniroot(function(hdi_width) get_percentage_hdi_outside_zero(hdi_width, results) - (0.05/n_tests),
                             lower = 0.9, upper = 1)$root},
                    error = function(e){return(NA)})
    
    p_effect_upper <- tryCatch({uniroot(function(upper_boundary) get_percentage_outside_p_effect_boundaries(upper_boundary, results) - (0.05/n_tests),
                                        lower = 0, upper = 1)$root},
                               error = function(e){return(NA)})
    
    sim_based_thresholds <- rbind(sim_based_thresholds, c(savage_dickey_bf, directional_bf_upper, directional_bf_lower, hdi, p_effect_upper))
  }
  sim_based_thresholds$n_tests <- seq(1:n_tests)
  colnames(sim_based_thresholds) <- c("savage_dickey_bf", "directional_bf_upper", "directional_bf_lower", "hdi", "p_effect_upper", "n_tests")
  
  return(sim_based_thresholds)
}


get_percentage_hdi_outside_zero <- function(hdi_width, results){
  hdis <- vector('list', nrow(results))
  counter <- 1
  
  for (i in 1:length(s_log_k_sds)){
    sd_path <- file.path("out", paste0("sd_", gsub("\\.", "_", s_log_k_sds[i])))
    
    for (j in 1:num_samples){
      sample_path <- file.path(sd_path, paste0("sample_", j))
      
      for (k in 1:length(prior_sds)){
        fit_path <- file.path(sample_path, paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".rds"))
        fit <- readRDS(fit_path)
        
        posterior_samples_mu_s_log_k <- extract(fit)$mu_s_log_k
        
        hdi_upper <- hdi(posterior_samples_mu_s_log_k, ci = hdi_width)$CI_high
        hdi_lower <- hdi(posterior_samples_mu_s_log_k, ci = hdi_width)$CI_low
        
        hdis[[counter]] <- c(hdi_upper, hdi_lower)
        counter <- counter + 1
      }
    }
  }
  hdis <- as.data.frame(do.call(rbind, hdis))
  colnames(hdis) <- c("hdi_upper", "hdi_lower")
  
  n_outside <- sum(hdis$hdi_upper < 0 | hdis$hdi_lower > 0)
  percentage <- n_outside / nrow(hdis)
  
  return(percentage)
}


get_percentage_outside_p_effect_boundaries <- function(upper_boundary, results){
  n_outside <- sum(results$p_effect < 1-upper_boundary | results$p_effect > upper_boundary)
  percentage <- n_outside / nrow(results)
  return(percentage)
}


sim_based_thresholds <- get_simulation_based_thresholds(df_results, 10)
saveRDS(sim_based_thresholds, "sim_based_thresholds.rds")
