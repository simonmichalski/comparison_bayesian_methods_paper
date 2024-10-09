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
#data_hdi_rope <- aggregate(prop_hdi_in_rope_conv ~ s_log_k_sd + prior_sd, df_results, mean)

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
  #geom_hline(yintercept = 3, linetype = 'dashed') +
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
  #geom_hline(yintercept = 3, linetype = 'dashed') +
  #geom_hline(yintercept = 0.33, linetype = 'dashed') +
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
  #geom_hline(yintercept = 0, linetype = 'dashed') +
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
  #geom_hline(yintercept = 0.95, linetype = 'dashed') +
  #geom_hline(yintercept = 0.05, linetype = 'dashed') +
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
data_fp_savage_dickey_bf_3 <- aggregate(fp_savage_dickey_bf_3 ~ prior_sd + s_log_k_sd, df_results, sum)

data_fp_directional_bf_3 <- aggregate(fp_directional_bf_3_pos + fp_directional_bf_1_3_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_3)[names(data_fp_directional_bf_3) == "fp_directional_bf_3_pos + fp_directional_bf_1_3_neg"] <- "fp_directional_bf_3"

data_fp_directional_bf_10 <- aggregate(fp_directional_bf_10_pos + fp_directional_bf_1_10_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_10)[names(data_fp_directional_bf_10) == "fp_directional_bf_10_pos + fp_directional_bf_1_10_neg"] <- "fp_directional_bf_10"

data_fp_directional_bf_30 <- aggregate(fp_directional_bf_30_pos + fp_directional_bf_1_30_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_30)[names(data_fp_directional_bf_30) == "fp_directional_bf_30_pos + fp_directional_bf_1_30_neg"] <- "fp_directional_bf_30"

data_fp_directional_bf_100 <- aggregate(fp_directional_bf_100_pos + fp_directional_bf_1_100_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_100)[names(data_fp_directional_bf_100) == "fp_directional_bf_100_pos + fp_directional_bf_1_100_neg"] <- "fp_directional_bf_100"

data_fp_hdi_80 <- aggregate(fp_hdi_80_pos + fp_hdi_80_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_hdi_80)[names(data_fp_hdi_80) == "fp_hdi_80_pos + fp_hdi_80_neg"] <- "fp_hdi_80"

data_fp_hdi_90 <- aggregate(fp_hdi_90_pos + fp_hdi_90_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_hdi_90)[names(data_fp_hdi_90) == "fp_hdi_90_pos + fp_hdi_90_neg"] <- "fp_hdi_90"

data_fp_hdi_95 <- aggregate(fp_hdi_95_pos + fp_hdi_95_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_hdi_95)[names(data_fp_hdi_95) == "fp_hdi_95_pos + fp_hdi_95_neg"] <- "fp_hdi_95"

data_fp_hdi_99 <- aggregate(fp_hdi_99_pos + fp_hdi_99_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_hdi_99)[names(data_fp_hdi_99) == "fp_hdi_99_pos + fp_hdi_99_neg"] <- "fp_hdi_99"

data_fp_p_effect_95 <- aggregate(fp_p_effect_95_pos + fp_p_effect_05_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_95)[names(data_fp_p_effect_95) == "fp_p_effect_95_pos + fp_p_effect_05_neg"] <- "fp_p_effect_95"

data_fp_p_effect_975 <- aggregate(fp_p_effect_975_pos + fp_p_effect_025_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_975)[names(data_fp_p_effect_975) == "fp_p_effect_975_pos + fp_p_effect_025_neg"] <- "fp_p_effect_975"

data_fp_p_effect_99 <- aggregate(fp_p_effect_99_pos + fp_p_effect_01_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_99)[names(data_fp_p_effect_99) == "fp_p_effect_99_pos + fp_p_effect_01_neg"] <- "fp_p_effect_99"


axis_text_size <- 4.5
axis_title_size <- 7
plot_title_size <- 6
border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3
line_width <- 1
hline_width <- 0.3

plot_fp_savage_dickey_bf_3 <- ggplot(data_fp_savage_dickey_bf_3, aes(x = as.factor(prior_sd), y = fp_savage_dickey_bf_3/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives", title = expression("Savage-Dickey" ~ BF[10] > 3)) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_95 <- ggplot(data_fp_p_effect_95, aes(x = as.factor(prior_sd), y = fp_p_effect_95/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.05, .95]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_975 <- ggplot(data_fp_p_effect_975, aes(x = as.factor(prior_sd), y = fp_p_effect_975/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.025, .975]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_99 <- ggplot(data_fp_p_effect_99, aes(x = as.factor(prior_sd), y = fp_p_effect_99/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.01, .99]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_3 <- ggplot(data_fp_directional_bf_3, aes(x = as.factor(prior_sd), y = fp_directional_bf_3/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(y = "Prop. false positives", title = expression("dBF"["+-"] ~~ "[1/3, 3]"), color = "Population SD") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.7,0.35),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.5, "lines"),
    legend.title = element_text(size = 5, margin = margin(b = 0)),
    legend.text = element_text(size = 5),
    legend.key.height = unit(0.2, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_10 <- ggplot(data_fp_directional_bf_10, aes(x = as.factor(prior_sd), y = fp_directional_bf_10/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(title = expression("dBF"["+-"] ~~ "[1/10, 10]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_30 <- ggplot(data_fp_directional_bf_30, aes(x = as.factor(prior_sd), y = fp_directional_bf_30/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(title = expression("dBF"["+-"] ~~ "[1/30, 30]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_100 <- ggplot(data_fp_directional_bf_100, aes(x = as.factor(prior_sd), y = fp_directional_bf_100/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(title = expression("dBF"["+-"] ~~ "[1/100, 100]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_80 <- ggplot(data_fp_hdi_80, aes(x = as.factor(prior_sd), y = fp_hdi_80/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives", title = "80% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_90 <- ggplot(data_fp_hdi_90, aes(x = as.factor(prior_sd), y = fp_hdi_90/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "90% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_95 <- ggplot(data_fp_hdi_95, aes(x = as.factor(prior_sd), y = fp_hdi_95/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "95% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_99 <- ggplot(data_fp_hdi_99, aes(x = as.factor(prior_sd), y = fp_hdi_99/100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "99% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )


multiplot_fp <- (plot_fp_savage_dickey_bf_3 | plot_fp_p_effect_95 | plot_fp_p_effect_975 | plot_fp_p_effect_99) /
  (plot_fp_dbf_3 | plot_fp_dbf_10 | plot_fp_dbf_30 | plot_fp_dbf_100) /
  (plot_fp_hdi_80 | plot_fp_hdi_90 | plot_fp_hdi_95 | plot_fp_hdi_99) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

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



