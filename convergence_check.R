library("rstan")

s_log_k_sds <- c(0.2)
num_samples <- 100
prior_sds <- c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)


convergence_check <- function(){
  for (i in 1:length(s_log_k_sds)){
    sd_path <- file.path("out", paste0("sd_", gsub("\\.", "_", s_log_k_sds[i])))
    
    for (j in 1:num_samples){
      sample_path <- file.path(sd_path, paste0("sample_", j))
      
      for (k in 1:length(prior_sds)){
        fit_path <- file.path(sample_path, paste0("model_prior_sd_", gsub("\\.", "_", prior_sds[k]), ".rds"))
        fit <- readRDS(fit_path)
        
        rhat_mu_s_log_k <- summary(fit)$summary[,'Rhat']
        
        if (any(round(rhat_mu_s_log_k, 2) < 1) || any(round(rhat_mu_s_log_k, 2) > 1.1)){
          print(paste("Model not converged:", fit_path))
        }
      }
    }
  }
}


convergence_check()
