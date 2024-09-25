library("rstan")
library("bayestestR")

s_log_k_sds <- c(0.2, 0.51, 0.81)
num_samples <- 100
prior_sds <- c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)


# dBF+-
get_directional_bf <- function(posterior_samples, prior_sd){
  prior <- distribution_normal(10000, 0, prior_sd)
  log_directional_bf <- bayesfactor_parameters(posterior_samples, null = c(-Inf, 0), prior = prior)$log_BF
  directional_bf <- exp(log_directional_bf)
  return(directional_bf)
}


# Savage-Dickey BF10
get_savage_dickey_bf <- function(posterior_samples, prior_sd){
  prior <- distribution_normal(10000, 0, prior_sd)
  log_savage_dickey_bf <- bayesfactor_parameters(posterior_samples, prior = prior)$log_BF
  savage_dickey_bf <- exp(log_savage_dickey_bf)
  return(savage_dickey_bf)
}


get_results_df <- function(){
  results <- vector('list', nrow(df_results))
  counter <- 1
  
  for (i in 1:length(s_log_k_sds)){
    sd_path <- file.path("out", paste0("sd_", gsub("\\.", "_", s_log_k_sds[i])))
    
    for (j in 1:num_samples){
      sample_path <- file.path(sd_path, paste0("sample_", j))
      
      for (k in 1:length(prior_sds)){
        fit_path <- file.path(sample_path, paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".rds"))
        fit <- readRDS(fit_path)
        
        posterior_samples_mu_s_log_k <- extract(fit)$mu_s_log_k
        
        directional_bf <- get_directional_bf(posterior_samples_mu_s_log_k, prior_sds[k])
        
        savage_dickey_bf <- get_savage_dickey_bf(posterior_samples_mu_s_log_k, prior_sds[k])
        
        p_effect <- sum(posterior_samples_mu_s_log_k > 0) / length(posterior_samples_mu_s_log_k)
        
        hdi_lower <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_low
        hdi_upper <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_high
        
        results[[counter]] <- c(s_log_k_sds[i], j, prior_sds[k], directional_bf, savage_dickey_bf, p_effect, hdi_lower, hdi_upper)
        counter <- counter + 1
      }
    }
  }
  
  results <- as.data.frame(do.call(rbind, results))
  colnames(results) <- c("s_log_k_sd", "sample", "prior_sd", "directional_bf", "savage_dickey_bf", "p_effect", "hdi_lower", "hdi_upper")
  
  # Add false positive (fp) result columns with conventional decision thresholds (conv)
  # Distinction between positive (pos) and negative (neg) false positive effects
  results$fp_savage_dickey_bf_conv <- ifelse(results$savage_dickey_bf > 3, 1, 0)
  results$fp_directional_bf_pos_conv <- ifelse(results$directional_bf > 3, 1, 0)
  results$fp_directional_bf_neg_conv <- ifelse(results$directional_bf < 0.33, 1, 0)
  results$fp_p_effect_pos_conv <- ifelse(results$p_effect > 0.975, 1, 0)
  results$fp_p_effect_neg_conv <- ifelse(results$p_effect < 0.025, 1, 0)
  results$fp_hdi_pos_conv <- ifelse(results$hdi_lower > 0, 1, 0)
  results$fp_hdi_neg_conv <- ifelse(results$hdi_upper < 0, 1, 0)
  
  return(results)
}


df_results <- get_results_df()
saveRDS(df_results, "final_results.rds")
