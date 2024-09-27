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
  results <- vector('list', length(s_log_k_sds)*num_samples*length(prior_sds))
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
        
        hdi_80_lower <- hdi(posterior_samples_mu_s_log_k, ci = 0.8)$CI_low
        hdi_80_upper <- hdi(posterior_samples_mu_s_log_k, ci = 0.8)$CI_high
        
        hdi_90_lower <- hdi(posterior_samples_mu_s_log_k, ci = 0.9)$CI_low
        hdi_90_upper <- hdi(posterior_samples_mu_s_log_k, ci = 0.9)$CI_high
        
        hdi_95_lower <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_low
        hdi_95_upper <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_high
        
        hdi_99_lower <- hdi(posterior_samples_mu_s_log_k, ci = 0.99)$CI_low
        hdi_99_upper <- hdi(posterior_samples_mu_s_log_k, ci = 0.99)$CI_high
        
        results[[counter]] <- c(s_log_k_sds[i], j, prior_sds[k], directional_bf, savage_dickey_bf, 
                                p_effect, hdi_80_lower, hdi_80_upper, hdi_90_lower, hdi_90_upper, 
                                hdi_95_lower, hdi_95_upper, hdi_99_lower, hdi_99_upper)
        
        print(paste0(counter, "/", length(s_log_k_sds)*num_samples*length(prior_sds)))
        counter <- counter + 1
      }
    }
  }
  
  results <- as.data.frame(do.call(rbind, results))
  colnames(results) <- c("s_log_k_sd", "sample", "prior_sd", "directional_bf", "savage_dickey_bf", 
                         "p_effect", "hdi_80_lower", "hdi_80_upper", "hdi_90_lower", "hdi_90_upper", 
                         "hdi_95_lower", "hdi_95_upper", "hdi_99_lower", "hdi_99_upper")
  
  # Add false positive (fp) result columns with conventional decision thresholds
  # Distinction between positive (pos) and negative (neg) false positive effects
  results$fp_savage_dickey_bf_3 <- ifelse(results$savage_dickey_bf > 3, 1, 0)
  
  results$fp_directional_bf_3_pos <- ifelse(results$directional_bf > 3, 1, 0)
  results$fp_directional_bf_1_3_neg <- ifelse(results$directional_bf < 1/3, 1, 0)
  results$fp_directional_bf_10_pos <- ifelse(results$directional_bf > 10, 1, 0)
  results$fp_directional_bf_1_10_neg <- ifelse(results$directional_bf < 1/10, 1, 0)
  results$fp_directional_bf_30_pos <- ifelse(results$directional_bf > 30, 1, 0)
  results$fp_directional_bf_1_30_neg <- ifelse(results$directional_bf < 1/30, 1, 0)
  results$fp_directional_bf_100_pos <- ifelse(results$directional_bf > 100, 1, 0)
  results$fp_directional_bf_1_100_neg <- ifelse(results$directional_bf < 1/100, 1, 0)
  
  results$fp_p_effect_95_pos <- ifelse(results$p_effect > 0.95, 1, 0)
  results$fp_p_effect_05_neg <- ifelse(results$p_effect < 0.05, 1, 0)
  results$fp_p_effect_975_pos <- ifelse(results$p_effect > 0.975, 1, 0)
  results$fp_p_effect_025_neg <- ifelse(results$p_effect < 0.025, 1, 0)
  results$fp_p_effect_99_pos <- ifelse(results$p_effect > 0.99, 1, 0)
  results$fp_p_effect_01_neg <- ifelse(results$p_effect < 0.01, 1, 0)
  
  results$fp_hdi_80_pos <- ifelse(results$hdi_80_lower > 0, 1, 0)
  results$fp_hdi_80_neg <- ifelse(results$hdi_80_upper < 0, 1, 0)
  results$fp_hdi_90_pos <- ifelse(results$hdi_90_lower > 0, 1, 0)
  results$fp_hdi_90_neg <- ifelse(results$hdi_90_upper < 0, 1, 0)
  results$fp_hdi_95_pos <- ifelse(results$hdi_95_lower > 0, 1, 0)
  results$fp_hdi_95_neg <- ifelse(results$hdi_95_upper < 0, 1, 0)
  results$fp_hdi_99_pos <- ifelse(results$hdi_99_lower > 0, 1, 0)
  results$fp_hdi_99_neg <- ifelse(results$hdi_99_upper < 0, 1, 0)
  
  return(results)
}


df_results <- get_results_df()
saveRDS(df_results, "final_results.rds")
