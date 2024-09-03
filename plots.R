library("rstan")
library("bayesplot")
library("ggplot2")
library("patchwork")

#fit <- readRDS("out/test3_sd_0_51/model_prior_sd_0_2.rds")

#R-hat plot
#traceplot(fit, pars = 'mu_s_log_k')

# Plot with HDI (richtig so?)
#posterior <- as.matrix(fit)
#mcmc_areas(posterior, pars = c('mu_s_log_k'), prob = 0.95)

df_results <- readRDS("final_results.rds")

# Savage-Dickey BFs, dBFs, P(effect > 0)
data_savage_dickey_bf <- aggregate(savage_dickey_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_directional_bf <- aggregate(directional_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_p_effect <- aggregate(p_effect ~ s_log_k_sd + prior_sd, df_results, mean)

plot_savage_dickey_bf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = savage_dickey_bf, color = as.factor(s_log_k_sd))) +
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

plot_dfb <- ggplot(df_results, aes(x = as.factor(prior_sd), y = directional_bf, color = as.factor(s_log_k_sd))) +
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

plot_p_effect <- ggplot(df_results, aes(x = as.factor(prior_sd), y = p_effect, color = as.factor(s_log_k_sd))) +
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


multiplot_values <- (plot_savage_dickey_bf | plot_dfb) + plot_p_effect +
  plot_layout(nrow = 2, ncol = 2) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))


# False positive results (conv)
data_fp_savage_dickey_bf_conv <- aggregate(fp_savage_dickey_bf_conv ~ prior_sd + s_log_k_sd, df_results, sum)
data_fp_p_effect_conv <- aggregate(fp_p_effect_upper_conv + fp_p_effect_lower_conv ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_conv)[names(data_fp_p_effect_conv) == "fp_p_effect_upper_conv + fp_p_effect_lower_conv"] <- "fp_p_effect_conv"
data_fp_directional_bf_conv <- aggregate(fp_directional_bf_upper_conv + fp_directional_bf_lower_conv ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_conv)[names(data_fp_directional_bf_conv) == "fp_directional_bf_upper_conv + fp_directional_bf_lower_conv"] <- "fp_directional_bf_conv"
data_fp_rope_conv <- aggregate(fp_rope_conv ~ prior_sd + s_log_k_sd, df_results, sum)

plot_fp_savage_dickey_bf <- ggplot(data_fp_savage_dickey_bf_conv, aes(x = as.factor(prior_sd), y = fp_savage_dickey_bf_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size = 7)
  ) +
  coord_cartesian(ylim = c(0, 0.13))

plot_fp_dbf <- ggplot(data_fp_directional_bf_conv, aes(x = as.factor(prior_sd), y = fp_directional_bf_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(color = "Population SD") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = c(0.75,0.75),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_text(size = 7, margin = margin(b = 0)),
    legend.text = element_text(size = 7),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
  ) +
  coord_cartesian(ylim = c(0, 0.13))

plot_fp_p_effect <- ggplot(data_fp_p_effect_conv, aes(x = as.factor(prior_sd), y = fp_p_effect_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  ) +
  coord_cartesian(ylim = c(0, 0.13))

plot_fp_rope <- ggplot(data_fp_rope_conv, aes(x = as.factor(prior_sd), y = fp_rope_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 7)
  ) +
  coord_cartesian(ylim = c(0, 0.13))


multiplot_fp <- plot_fp_savage_dickey_bf + plot_fp_dbf + plot_fp_p_effect + plot_fp_rope +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))






