library("rstan")
library("bayestestR")

s_log_k_sds <- c(0.2, 0.51, 0.81)
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

#### ROPE definition!!!!!; rhat criterion!!!!
get_results <- function(){
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
          hdi_low <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_low
          hdi_high <- hdi(posterior_samples_mu_s_log_k, ci = 0.95)$CI_high
          prop_hdi_in_rope <- rope(posterior_samples_mu_s_log_k, range = c(-0.1, 0.1), ci = 0.95)$ROPE_Percentage
          
          results <- rbind(results, c(s_log_k_sds[i], j, prior_sds[k], directional_bf, savage_dickey_bf, p_effect, hdi_low, hdi_high, prop_hdi_in_rope))
          colnames(results) <- c("s_log_k_sd", "sample", "prior_sd", "directional_bf", "savage_dickey_bf", "p_effect", "hdi_low", "hdi_high", "prop_hdi_in_rope")
        }
      }
    }
  }
  return(results)
}

