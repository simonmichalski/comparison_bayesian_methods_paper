library("rstan")
library("bayestestR")

s_log_k_sds <- c(0.51, 0.81)
num_samples <- 100
prior_sds <- c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)


# dBF+-
get_directional_bf <- function(posterior_samples){
  density_pos <- density(posterior_samples, from = 0, to = 999)
  density_neg <- density(posterior_samples, from = -999, to = 0)
  directional_bf <- sum(density_pos$y) / sum(density_neg$y)
  return(directional_bf)
}


# Savage-Dickey BF10
get_savage_dickey_bf <- function(prior_sd, posterior_samples){
  prior <- rnorm(10000, 0, prior_sd)
  savage_dickey_bf <- exp(bayesfactor_parameters(posterior_samples, prior = prior)$log_BF)
  return(savage_dickey_bf)
}


get_results_df <- function(){
  results <- data.frame()
  
  for (i in 1:length(s_log_k_sds)){
    sd_path <- file.path("out", paste0("sd_", gsub("\\.", "_", s_log_k_sds[i])))
    
    for (j in 1:num_samples){
      sample_path <- file.path(sd_path, paste0("sample_", j))
      
      for (k in 1:length(prior_sds)){
        fit_path <- file.path(sample_path, paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".rds"))
        fit <- readRDS(fit_path)
        
        rhat_mu_s_log_k <- summary(fit)$summary['mu_s_log_k', 'Rhat']
        
        if (round(rhat_mu_s_log_k, 2) < 1 || round(rhat_mu_s_log_k, 2) > 1.1){
          print(paste("Model not converged:", fit_path))
        }
        else {
          posterior_samples_mu_s_log_k <- extract(fit)$mu_s_log_k
          
          directional_bf <- get_directional_bf(posterior_samples_mu_s_log_k)
          
          savage_dickey_bf <- get_savage_dickey_bf(prior_sds[k], posterior_samples_mu_s_log_k)
          
          p_effect <- sum(posterior_samples_mu_s_log_k > 0) / length(posterior_samples_mu_s_log_k)
          
          hdi_lower <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_low
          hdi_upper <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_high
          
          rope_bounds <- s_log_k_sds[i]*0.1
          prop_hdi_in_rope <- rope(posterior_samples_mu_s_log_k, range = c(-rope_bounds, rope_bounds), ci = 0.95)$ROPE_Percentage
          
          results <- rbind(results, c(s_log_k_sds[i], j, prior_sds[k], directional_bf, savage_dickey_bf, p_effect, hdi_lower, hdi_upper, prop_hdi_in_rope))
          colnames(results) <- c("s_log_k_sd", "sample", "prior_sd", "directional_bf", "savage_dickey_bf", "p_effect", "hdi_lower", "hdi_upper", "prop_hdi_in_rope_conv")
        }
      }
    }
  }
  sim_thresholds <- get_simulation_based_thresholds(results)
  
  # Add false positive result columns with conventional decision thresholds
  results$fp_directional_bf_upper_conv <- ifelse(results$directional_bf > 3, 1, 0)
  results$fp_directional_bf_lower_conv <- ifelse(results$directional_bf < 0.33, 1, 0)
  results$fp_savage_dickey_bf_conv <- ifelse(results$savage_dickey_bf > 3, 1, 0)
  results$fp_p_effect_upper_conv <- ifelse(results$p_effect > 0.95, 1, 0)
  results$fp_p_effect_lower_conv <- ifelse(results$p_effect < 0.05, 1, 0)
  results$fp_rope_conv <- ifelse(results$prop_hdi_in_rope_conv == 0, 1, 0)
  
  # Add false positive result columns with simulation-based decision thresholds
  results$fp_directional_bf_upper_sim <- ifelse(results$directional_bf > sim_thresholds$directional_bf_upper, 1, 0)
  results$fp_directional_bf_lower_sim <- ifelse(results$directional_bf < sim_thresholds$directional_bf_lower, 1, 0)
  results$fp_savage_dickey_bf_sim <- ifelse(results$savage_dickey_bf > sim_thresholds$savage_dickey_bf, 1, 0)
  results$fp_p_effect_upper_sim <- ifelse(results$p_effect > sim_thresholds$p_effect_upper, 1, 0)
  results$fp_p_effect_lower_sim <- ifelse(results$p_effect < 1 - sim_thresholds$p_effect_upper, 1, 0)
  # add if simulation-based ROPE necessary
  results$fp_rope_sim <- NA
  
  return(results)
}


get_percentage_outside_of_rope <- function(rope_boundary, results){
  n_outside <- sum(results$hdi_upper < -rope_boundary | results$hdi_lower > rope_boundary)
  percentage <- n_outside / nrow(results)
  return(percentage)
}


get_percentage_outside_p_effect_boundaries <- function(upper_boundary, results){
  n_outside <- sum(results$p_effect < 1-upper_boundary | results$p_effect > upper_boundary)
  percentage <- n_outside / nrow(results)
  return(percentage)
}


get_simulation_based_thresholds <- function(results){
  directional_bf_upper <- quantile(results$directional_bf, probs = 0.975)[[1]]
  directional_bf_lower <- quantile(results$directional_bf, probs = 0.025)[[1]]
  
  savage_dickey_bf <- quantile(results$savage_dickey_bf, probs = 0.95)[[1]]
  
  p_effect_upper <- tryCatch({uniroot(function(upper_boundary) get_percentage_outside_p_effect_boundaries(upper_boundary, results) - 0.05,
                             lower = 0, upper = 1)$root},
                             error = function(e){return(NA)})
  
  rope <- tryCatch({uniroot(function(rope_boundary) get_percentage_outside_of_rope(rope_boundary, results) - 0.05,
                            lower = 0, upper = max(abs(results$hdi_lower), abs(results$hdi_upper)))$root},
                   error = function(e){return(NA)})
  
  sim_thresholds <- list(directional_bf_upper = directional_bf_upper, 
                         directional_bf_lower = directional_bf_lower,
                         savage_dickey_bf = savage_dickey_bf,
                         p_effect_upper = p_effect_upper,
                         rope = rope)
  return(sim_thresholds)
}


df_results <- get_results_df()
saveRDS(df_results, "test_results.rds")
