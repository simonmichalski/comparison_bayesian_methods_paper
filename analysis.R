library("rstan")
library("bayestestR")

s_log_k_sds <- c(0.2, 0.51, 0.81)
num_samples <- 200
prior_sds <- c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)

n_models <- length(s_log_k_sds)*num_samples*length(prior_sds)

hdi_seq <- seq(from = 0.9, to = 1, by = 0.0001)
n_tests <- 10


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


# Compute inference methods for every model (Savage-Dickey BF, dBF, P(effect > 0), 80% HDI, 90% HDI, 95% HDI, 99% HDI)
get_results <- function(){
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
        mu_s_log_k <- summary(fit)$summary[['mu_s_log_k', 'mean']]
        remove(fit)
        
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
        
        results[[counter]] <- c(s_log_k_sds[i], j, prior_sds[k], mu_s_log_k, directional_bf, savage_dickey_bf, 
                                p_effect, hdi_80_lower, hdi_80_upper, hdi_90_lower, hdi_90_upper, 
                                hdi_95_lower, hdi_95_upper, hdi_99_lower, hdi_99_upper)
        
        print(paste0(counter, "/", length(s_log_k_sds)*num_samples*length(prior_sds)))
        counter <- counter + 1
      }
    }
  }
  results <- as.data.frame(do.call(rbind, results))
  colnames(results) <- c("s_log_k_sd", "sample", "prior_sd", "mu_s_log_k", "directional_bf", "savage_dickey_bf", 
                         "p_effect", "hdi_80_lower", "hdi_80_upper", "hdi_90_lower", "hdi_90_upper", 
                         "hdi_95_lower", "hdi_95_upper", "hdi_99_lower", "hdi_99_upper")
  return(results)
}


# Compute upper and lower bounds of all HDIs in hdi_seq for every model
get_hdi_bounds <- function(){
  model_hdis <- vector('list', n_models)
  model_counter <- 1
  
  for (i in 1:length(s_log_k_sds)){
    
    for (j in 1:num_samples){
      
      for (k in 1:length(prior_sds)){
        fit_path <- file.path("out", paste0("sd_", gsub("\\.", "_", s_log_k_sds[i])), paste0("sample_", j), paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".rds"))
        fit <- readRDS(fit_path)
        
        posterior_samples_mu_s_log_k <- extract(fit)$mu_s_log_k
        remove(fit)
        
        hdis <- vector('list', length(hdi_seq))
        hdi_counter <- 1
        for (hdi_width in hdi_seq){
          hdi_lower <- hdi(posterior_samples_mu_s_log_k, ci = hdi_width)$CI_low
          hdi_upper <- hdi(posterior_samples_mu_s_log_k, ci = hdi_width)$CI_high
          
          hdis[[hdi_counter]] <- c(hdi_lower, hdi_upper)
          hdi_counter <- hdi_counter + 1
        }
        
        model_hdis[[model_counter]] <- hdis
        print(paste0(model_counter, "/", n_models))
        model_counter <- model_counter + 1
      }
    }
  }
  return(model_hdis)
}


# Compute simulation-based decision thresholds
get_simulation_based_thresholds <- function(results, model_hdis){
  sim_based_thresholds <- data.frame()
  
  for (i in 1:n_tests){
    savage_dickey_bf <- quantile(results$savage_dickey_bf, probs = 1-(0.05/i))[[1]]
    
    directional_bf_upper <- quantile(results$directional_bf, probs = 1-(0.025/i))[[1]]
    directional_bf_lower <- quantile(results$directional_bf, probs = 0.025/i)[[1]]
    
    p_effect_upper <- quantile(results$p_effect, probs = 1-(0.025/i))[[1]]
    p_effect_lower <- quantile(results$p_effect, probs = 0.025/i)[[1]]
    
    sim_based_thresholds <- rbind(sim_based_thresholds, c(i, savage_dickey_bf, directional_bf_upper, directional_bf_lower, p_effect_upper, p_effect_lower))
  }
  colnames(sim_based_thresholds) <- c("n_tests", "savage_dickey_bf", "directional_bf_upper", "directional_bf_lower", "p_effect_upper", "p_effect_lower")
  
  sim_based_thresholds$hdi <- get_sim_based_hdi_widths(model_hdis)
  
  return(sim_based_thresholds)
}


get_sim_based_hdi_widths <- function(model_hdis){
  proportions_false_positives <- c()
  
  for (hdi in 1:length(hdi_seq)){
    num_false_positives <- 0
    
    for (model in 1:n_models){
      hdi_lower <- model_hdis[[model]][[hdi]][1]
      hdi_upper <- model_hdis[[model]][[hdi]][2]
      
      num_false_positives <- num_false_positives + ifelse(hdi_upper < 0 | hdi_lower > 0, 1, 0)
    }
    proportions_false_positives <- append(proportions_false_positives, num_false_positives/n_models)
  }
  
  sim_based_hdis <- c()
  
  for (tests in 1:n_tests){
    # Get indices of relevant false positive proportions
    indices <- which(proportions_false_positives <= (0.05/tests))
    # Get index of closest false positive proportion
    hdi_seq_index <- indices[which.min(abs(proportions_false_positives[indices] - (0.05/tests)))]
    sim_based_hdis <- append(sim_based_hdis, hdi_seq[hdi_seq_index])
  }
  
  return(sim_based_hdis)
}


# Add false positive (fp) result columns with conventional and simulation-based (sim) decision thresholds
# Distinction between positive (pos) and negative (neg) false positive effects
add_false_positive_results_columns <- function(results, model_hdis, sim_thres){
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
  
  results$fp_savage_dickey_bf_sim <- ifelse(results$savage_dickey_bf > sim_thres$savage_dickey_bf[1], 1, 0)
  results$fp_directional_bf_sim_pos <- ifelse(results$directional_bf > sim_thres$directional_bf_upper[1], 1, 0)
  results$fp_directional_bf_sim_neg <- ifelse(results$directional_bf < sim_thres$directional_bf_lower[1], 1, 0)
  results$fp_p_effect_sim_pos <- ifelse(results$p_effect > sim_thres$p_effect_upper[1], 1, 0)
  results$fp_p_effect_sim_neg <- ifelse(results$p_effect < sim_thres$p_effect_lower[1], 1, 0)
  
  fp_hdi_sim_pos <- c()
  fp_hdi_sim_neg <- c()
  
  for (i in 1:n_models){
    seq_index <- which(hdi_seq == sim_thres$hdi[1])
    lower_bounds <- model_hdis[[i]][[seq_index]][1]
    upper_bounds <- model_hdis[[i]][[seq_index]][2]
    fp_hdi_sim_pos <- append(fp_hdi_sim_pos, ifelse(lower_bounds > 0, 1, 0))
    fp_hdi_sim_neg <- append(fp_hdi_sim_neg, ifelse(upper_bounds < 0, 1, 0))
  }
  
  results$fp_hdi_sim_pos <- fp_hdi_sim_pos
  results$fp_hdi_sim_neg <- fp_hdi_sim_neg
  
  return(results)
}


results <- get_results()
hdi_bounds <- get_hdi_bounds()
sim_based_thresholds <- get_simulation_based_thresholds(results, hdi_bounds)
final_results <- add_false_positive_results_columns(results, hdi_bounds, sim_based_thresholds)

saveRDS(results, "final_results/results.rds")
saveRDS(hdi_bounds, "final_results/hdi_bounds.rds")
saveRDS(sim_based_thresholds, "final_results/sim_based_thresholds.rds")
saveRDS(final_results, "final_results/final_results.rds")
