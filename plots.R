library("rstan")
library("bayesplot")
library("ggplot2")
library("patchwork")
library("dplyr")

#fit <- readRDS("out/test3_sd_0_51/model_prior_sd_0_2.rds")

#R-hat plot
#traceplot(fit, pars = 'mu_s_log_k')

# Plot with HDI (richtig so?)
#posterior <- as.matrix(fit)
#mcmc_areas(posterior, pars = c('mu_s_log_k'), prob = 0.95)

df_results <- readRDS("final_results.rds")

# Values
data_savage_dickey_bf <- aggregate(savage_dickey_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_directional_bf <- aggregate(directional_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_p_effect <- aggregate(p_effect ~ s_log_k_sd + prior_sd, df_results, mean)
data_hdi_rope <- aggregate(prop_hdi_in_rope_conv ~ s_log_k_sd + prior_sd, df_results, mean)

dodge_width <- 0.7
line_width <- 1

plot_savage_dickey_bf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = savage_dickey_bf, color = as.factor(s_log_k_sd))) +
  labs(x = "", y = expression("Savage-Dickey BF"[10]), color = "Population SD") +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5
  ) +
  geom_line(data = data_savage_dickey_bf,
            aes(x = as.factor(prior_sd), 
                y = savage_dickey_bf, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 3, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = c(0.75,0.75),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_text(size = 7, margin = margin(b = 0)),
    legend.text = element_text(size = 7),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7)
  )

plot_dbf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = directional_bf, color = as.factor(s_log_k_sd))) +
  labs(x = "", y = expression("dBF"["+-"])) +
  scale_y_log10(labels = scales::comma) +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5
  ) +
  geom_line(data = data_directional_bf,
            aes(x = as.factor(prior_sd), 
                y = directional_bf, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 3, linetype = 'dashed') +
  geom_hline(yintercept = 0.33, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_blank()
  )

plot_hdi_rope <- ggplot(df_results, aes(x = as.factor(prior_sd), y = prop_hdi_in_rope_conv, color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. 95% HDI in ROPE") +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5
  ) +
  geom_line(data = data_hdi_rope,
            aes(x = as.factor(prior_sd), 
                y = prop_hdi_in_rope_conv, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7)
  )

plot_p_effect <- ggplot(df_results, aes(x = as.factor(prior_sd), y = p_effect, color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "P(effect > 0)") +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5
  ) +
  geom_line(data = data_p_effect,
            aes(x = as.factor(prior_sd), 
                y = p_effect, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0.95, linetype = 'dashed') +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7)
  )


multiplot_values <- (plot_savage_dickey_bf + plot_dbf) / (plot_hdi_rope + plot_p_effect) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))

ggsave("plots/multiplot_values.pdf", plot = multiplot_values, width = 6, height = 4, units = "in", dpi = 300)


# False positive results (conv)
data_fp_savage_dickey_bf_conv <- aggregate(fp_savage_dickey_bf_conv ~ prior_sd + s_log_k_sd, df_results, sum)
data_fp_p_effect_conv <- aggregate(fp_p_effect_upper_conv + fp_p_effect_lower_conv ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_conv)[names(data_fp_p_effect_conv) == "fp_p_effect_upper_conv + fp_p_effect_lower_conv"] <- "fp_p_effect_conv"
data_fp_directional_bf_conv <- aggregate(fp_directional_bf_upper_conv + fp_directional_bf_lower_conv ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_conv)[names(data_fp_directional_bf_conv) == "fp_directional_bf_upper_conv + fp_directional_bf_lower_conv"] <- "fp_directional_bf_conv"
data_fp_rope_conv <- aggregate(fp_rope_conv ~ prior_sd + s_log_k_sd, df_results, sum)

plot_fp_savage_dickey_bf <- ggplot(data_fp_savage_dickey_bf_conv, aes(x = as.factor(prior_sd), y = fp_savage_dickey_bf_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives", color = "Population SD", title = expression("Savage-Dickey BF"[10])) +
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
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 9)
  ) +
  coord_cartesian(ylim = c(0, 0.13))

plot_fp_dbf <- ggplot(data_fp_directional_bf_conv, aes(x = as.factor(prior_sd), y = fp_directional_bf_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(y = "Prop. false positives", title = expression("dBF"["+-"])) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 9)
  )

plot_fp_p_effect <- ggplot(data_fp_p_effect_conv, aes(x = as.factor(prior_sd), y = fp_p_effect_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives", title = "P(effect > 0)") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 9)
  ) +
  coord_cartesian(ylim = c(0, 0.13))

plot_fp_rope <- ggplot(data_fp_rope_conv, aes(x = as.factor(prior_sd), y = fp_rope_conv/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives", title = "95% HDI + ROPE") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.ticks.length = unit(-0.1, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 9)
  ) +
  coord_cartesian(ylim = c(0, 0.13))


multiplot_fp <- plot_fp_savage_dickey_bf + plot_fp_dbf + plot_fp_p_effect + plot_fp_rope +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))

ggsave("plots/multiplot_fp.pdf", plot = multiplot_fp, width = 6, height = 4, units = "in", dpi = 300)


# Simulation-based decision thresholds (sbdt)
sim_based_thresholds <- readRDS("sim_based_thresholds.rds")

data_sbdt_directional_bf <- pivot_longer(sim_based_thresholds[,c("directional_bf_upper", "directional_bf_lower", "n_tests")], 
                                         cols = starts_with("directional_bf"),
                                         names_to = "threshold", values_to = "value")

plot_sbdt_savage_dickey_bf <- ggplot(sim_based_thresholds, aes(x = as.factor(n_tests), y = value, group = as.factor(threshold))) +
  labs(x = "n tests", y = expression("Savage-Dickey BF"[10])) +
  geom_line(linewidth = 1)

plot_sbdt_directional_bf <- ggplot(data_sbdt_directional_bf, aes(x = as.factor(n_tests), y = directional_bf, group = as.factor(upper_lower)))

plot_sbdt_hdi_bf <- ggplot(data_sbdt_hdi, aes(x = as.factor(n_tests), y = hdi))

plot_sbdt_p_effect_bf <- ggplot(data_sbdt_p_effect, aes(x = as.factor(n_tests), y = p_effect))



