library("rstan")
library("bayesplot")
library("ggplot2")

#fit <- readRDS("out/test3_sd_0_51/model_prior_sd_0_2.rds")

#R-hat plot
#traceplot(fit, pars = 'mu_s_log_k')

# Plot with HDI (richtig so?)
#posterior <- as.matrix(fit)
#mcmc_areas(posterior, pars = c('mu_s_log_k'), prob = 0.95)

df_results <- readRDS("test_results.rds")

# Savage-Dickey BFs, dBFs, P(effect > 0)
data_savage_dickey_bf <- aggregate(savage_dickey_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_directional_bf <- aggregate(directional_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_p_effect <- aggregate(p_effect ~ s_log_k_sd + prior_sd, df_results, mean)

ggplot(df_results, aes(x = as.factor(prior_sd), y = savage_dickey_bf, color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = expression("Savage-Dickey BF "[10])) +
  geom_hline(yintercept = 3, linetype = 'dashed') +
  geom_line(data = data_savage_dickey_bf, 
            aes(x = as.factor(prior_sd), 
                y = savage_dickey_bf, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd))) +
  geom_jitter(
    position = position_dodge(width = 0.5),
    alpha = 0.5) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none')

ggplot(df_results, aes(x = as.factor(prior_sd), y = directional_bf, color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = expression("dBF"["+-"])) +
  geom_hline(yintercept = 3, linetype = 'dashed') +
  geom_hline(yintercept = 0.33, linetype = 'dashed') +
  geom_line(data = data_directional_bf, 
            aes(x = as.factor(prior_sd), 
                y = directional_bf, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd))) +
  geom_jitter(
    position = position_dodge(width = 0.5),
    alpha = 0.5
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none'
  )

ggplot(df_results, aes(x = as.factor(prior_sd), y = p_effect, color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "P(effect > 0)") +
  geom_hline(yintercept = 0.95, linetype = 'dashed') +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  geom_line(data = data_p_effect, 
            aes(x = as.factor(prior_sd), 
                y = p_effect, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd))) +
  geom_jitter(
    position = position_dodge(width = 0.5),
    alpha = 0.5
    ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none'
  )


# False positive results (conv)
data_fp_savage_dickey_bf_conv <- aggregate(fp_savage_dickey_bf_conv ~ prior_sd + s_log_k_sd, df_results, sum)
data_fp_p_effect_conv <- aggregate(fp_p_effect_upper_conv + fp_p_effect_lower_conv ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_conv)[names(data_fp_p_effect_conv) == "fp_p_effect_upper_conv + fp_p_effect_lower_conv"] <- "fp_p_effect_conv"
data_fp_directional_bf_conv <- aggregate(fp_directional_bf_upper_conv + fp_directional_bf_lower_conv ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_conv)[names(data_fp_directional_bf_conv) == "fp_directional_bf_upper_conv + fp_directional_bf_lower_conv"] <- "fp_directional_bf_conv"
data_fp_rope_conv <- aggregate(fp_rope_conv ~ prior_sd + s_log_k_sd, df_results, sum)

ggplot(data_fp_savage_dickey_bf_conv, aes(x = as.factor(prior_sd), y = fp_savage_dickey_bf_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Proportion false positive results") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none'
  )

ggplot(data_fp_directional_bf_conv, aes(x = as.factor(prior_sd), y = fp_directional_bf_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Proportion false positive results") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none'
  )

ggplot(data_fp_p_effect_conv, aes(x = as.factor(prior_sd), y = fp_p_effect_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Proportion false positive results") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none'
  )

ggplot(data_fp_rope_conv, aes(x = as.factor(prior_sd), y = fp_rope_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Proportion false positive results") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none'
  )








