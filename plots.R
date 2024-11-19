library("bayesplot")
library("ggplot2")
library("patchwork")
library("tidyr")
library("dplyr")
library("rstan")

if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

df_results <- readRDS("final_results/final_results.rds")
df_sim_thres <- readRDS("final_results/sim_based_thresholds.rds")
df_recovery <- readRDS("final_results/recovery.rds")

# Values
data_savage_dickey_bf <- aggregate(savage_dickey_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_directional_bf <- aggregate(directional_bf ~ s_log_k_sd + prior_sd, df_results, mean)
data_p_effect <- aggregate(p_effect ~ s_log_k_sd + prior_sd, df_results, mean)

dodge_width <- 0.8
point_size <- 0.5
axis_text_size <- 4.5
axis_title_size <- 7
border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3
line_width <- 1

plot_savage_dickey_bf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = savage_dickey_bf, color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior " * italic("SD")), y = expression("Savage-Dickey BF"[10]), color = expression("Population " * italic("SD"))) +
  scale_y_log10(labels = scales::comma) +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5,
    size = point_size
  ) +
  geom_line(data = data_savage_dickey_bf,
            aes(x = as.factor(prior_sd), 
                y = savage_dickey_bf, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks.length = unit(tick_length, 'cm'),
    axis.ticks = element_line(linewidth = tick_width),
    legend.position = c(0.2,0.15),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.4, "lines"),
    legend.title = element_text(size = 4, margin = margin(b = 0)),
    legend.text = element_text(size = 4),
    legend.key.height = unit(0.1, "cm"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_dbf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = directional_bf, color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior " * italic("SD")), y = expression("dBF"["+-"])) +
  scale_y_log10(labels = scales::comma) +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5,
    size = point_size
  ) +
  geom_line(data = data_directional_bf,
            aes(x = as.factor(prior_sd), 
                y = directional_bf, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks.length = unit(tick_length, 'cm'),
    axis.ticks = element_line(linewidth = tick_width),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )


plot_p_effect <- ggplot(df_results, aes(x = as.factor(prior_sd), y = p_effect, color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior " * italic("SD")), y = "P(effect > 0)") +
  geom_jitter(
    position = position_dodge(width = dodge_width),
    alpha = 0.5,
    size = point_size
  ) +
  geom_line(data = data_p_effect,
            aes(x = as.factor(prior_sd), 
                y = p_effect, 
                color = as.factor(s_log_k_sd), 
                group = as.factor(s_log_k_sd)),
            linewidth = line_width,
            position = position_dodge(width = dodge_width)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks.length = unit(tick_length, 'cm'),
    axis.ticks = element_line(linewidth = tick_width),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )


multiplot_values <- plot_savage_dickey_bf + plot_dbf + plot_p_effect +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))

ggsave("plots/multiplot_values.pdf", plot = multiplot_values, width = 6, height = 2, units = "in", dpi = 300)


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

plot_fp_savage_dickey_bf_3 <- ggplot(data_fp_savage_dickey_bf_3, aes(x = as.factor(prior_sd), y = fp_savage_dickey_bf_3/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_95 <- ggplot(data_fp_p_effect_95, aes(x = as.factor(prior_sd), y = fp_p_effect_95/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_975 <- ggplot(data_fp_p_effect_975, aes(x = as.factor(prior_sd), y = fp_p_effect_975/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_99 <- ggplot(data_fp_p_effect_99, aes(x = as.factor(prior_sd), y = fp_p_effect_99/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_3 <- ggplot(data_fp_directional_bf_3, aes(x = as.factor(prior_sd), y = fp_directional_bf_3/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_10 <- ggplot(data_fp_directional_bf_10, aes(x = as.factor(prior_sd), y = fp_directional_bf_10/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_30 <- ggplot(data_fp_directional_bf_30, aes(x = as.factor(prior_sd), y = fp_directional_bf_30/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_100 <- ggplot(data_fp_directional_bf_100, aes(x = as.factor(prior_sd), y = fp_directional_bf_100/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_80 <- ggplot(data_fp_hdi_80, aes(x = as.factor(prior_sd), y = fp_hdi_80/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_90 <- ggplot(data_fp_hdi_90, aes(x = as.factor(prior_sd), y = fp_hdi_90/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_95 <- ggplot(data_fp_hdi_95, aes(x = as.factor(prior_sd), y = fp_hdi_95/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_99 <- ggplot(data_fp_hdi_99, aes(x = as.factor(prior_sd), y = fp_hdi_99/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
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
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )


multiplot_fp <- (plot_fp_savage_dickey_bf_3 | plot_fp_p_effect_95 | plot_fp_p_effect_975 | plot_fp_p_effect_99) /
  (plot_fp_dbf_3 | plot_fp_dbf_10 | plot_fp_dbf_30 | plot_fp_dbf_100) /
  (plot_fp_hdi_80 | plot_fp_hdi_90 | plot_fp_hdi_95 | plot_fp_hdi_99) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave("plots/multiplot_fp.pdf", plot = multiplot_fp, width = 6, height = 4, units = "in", dpi = 300)


# False positive results (sim)
data_fp_savage_dickey_bf_sim <- aggregate(fp_savage_dickey_bf_sim ~ prior_sd + s_log_k_sd, df_results, sum)

data_fp_directional_bf_sim <- aggregate(fp_directional_bf_sim_pos + fp_directional_bf_sim_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_sim)[names(data_fp_directional_bf_sim) == "fp_directional_bf_sim_pos + fp_directional_bf_sim_neg"] <- "fp_directional_bf_sim"

data_fp_hdi_sim <- aggregate(fp_hdi_sim_pos + fp_hdi_sim_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_hdi_sim)[names(data_fp_hdi_sim) == "fp_hdi_sim_pos + fp_hdi_sim_neg"] <- "fp_hdi_sim"

data_fp_p_effect_sim <- aggregate(fp_p_effect_sim_pos + fp_p_effect_sim_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_p_effect_sim)[names(data_fp_p_effect_sim) == "fp_p_effect_sim_pos + fp_p_effect_sim_neg"] <- "fp_p_effect_sim"


axis_text_size <- 4.5
axis_title_size <- 7
plot_title_size <- 6
border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3
line_width <- 1
hline_width <- 0.3

plot_fp_savage_dickey_bf_sim <- ggplot(data_fp_savage_dickey_bf_sim, aes(x = as.factor(prior_sd), y = fp_savage_dickey_bf_sim/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Prop. false positives", title = expression("Savage-Dickey" ~ BF[10] > 1.26)) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_sim <- ggplot(data_fp_directional_bf_sim, aes(x = as.factor(prior_sd), y = fp_directional_bf_sim/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = expression("dBF"["+-"] ~~ "[0.04, 24.14]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_sim <- ggplot(data_fp_hdi_sim, aes(x = as.factor(prior_sd), y = fp_hdi_sim/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "92.46% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_sim <- ggplot(data_fp_p_effect_sim, aes(x = as.factor(prior_sd), y = fp_p_effect_sim/200, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.04, .96]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 0.05, linetype = 'dashed', linewidth = hline_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

multiplot_fp_sim <- (plot_fp_savage_dickey_bf_sim | plot_fp_dbf_sim | plot_fp_hdi_sim | plot_fp_p_effect_sim) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave("plots/multiplot_fp_sim.pdf", plot = multiplot_fp_sim, width = 6, height = 4/3, units = "in", dpi = 300)


# Simulation-based decision thresholds (sbdt)
axis_text_size <- 4.5
axis_title_size <- 7
border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3
line_width <- 1

plot_sbdt_savage_dickey_bf <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = savage_dickey_bf, group = 1)) +
  labs(y = expression("Savage-Dickey BF"[10])) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_sbdt_directional_bf_lower <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = directional_bf_lower, group = 1)) +
  labs(y = expression("dBF"["+-"] ~~ "lower")) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_sbdt_directional_bf_upper <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = directional_bf_upper, group = 1)) +
  labs(y = expression("dBF"["+-"] ~~ "upper")) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
  
plot_sbdt_hdi_bf <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = hdi*100, group = 1)) +
  labs(x = "n tests", y = "HDI width") +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_sbdt_p_effect_bf_lower <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = p_effect_lower, group = 1)) +
  labs(x = "n tests", y = "P(effect > 0) lower") +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_sbdt_p_effect_bf_upper <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = p_effect_upper, group = 1)) +
  labs(x = "n tests", y = "P(effect > 0) upper") +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )


multiplot_fp <- (plot_sbdt_savage_dickey_bf | plot_sbdt_directional_bf_lower | plot_sbdt_directional_bf_upper) / 
  (plot_sbdt_hdi_bf | plot_sbdt_p_effect_bf_lower | plot_sbdt_p_effect_bf_upper) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave("plots/multiplot_thresholds.pdf", plot = multiplot_fp, width = 6, height = 2.66, units = "in", dpi = 300)


# Parameter recovery
data_recovery_log_k <- summarize(group_by(df_recovery, s_log_k_sd, prior_sd, sample), 
                                 correlation = cor(true_log_k, median_log_k))

data_recovery_log_k <- aggregate(correlation ~ prior_sd + s_log_k_sd, data_recovery_log_k, mean)


df_recovery$diff_s_log_k <- abs(df_recovery$median_s_log_k - df_recovery$true_s_log_k)

data_cor_s_log_k <- summarize(group_by(df_recovery, s_log_k_sd, prior_sd), 
                              correlation = cor(true_s_log_k, median_s_log_k))

data_diff_s_log_k <- aggregate(diff_s_log_k ~ prior_sd + s_log_k_sd, df_recovery, mean)

axis_text_size <- 7
axis_title_size <- 10
border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3
line_width <- 1
point_size <- 1.5
plot_title_size <- 10

scatter_log_k <- ggplot(df_recovery, aes(x = true_log_k, y = median_log_k)) +
  labs(x = "True value", y = "Estimate", title = expression("log("*italic(k)*")")) +
  geom_jitter(
    alpha = 0.1,
    size = point_size
  ) +
  scale_x_continuous(limits = c(-9,0)) +
  scale_y_continuous(limits = c(-9,0)) +
  geom_abline(slope = 1, intercept = 0, linewidth = border_size) +
  coord_fixed(ratio = 1) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

scatter_s_log_k <- ggplot(df_recovery, aes(x = true_s_log_k, y = median_s_log_k)) +
  labs(x = "True value", y = "Estimate", title = expression("s"["log("*italic(k)*")"])) +
  geom_jitter(
    size = 0.1
  ) +
  scale_x_continuous(limits = c(-3,4.5)) +
  scale_y_continuous(limits = c(-3,4.5)) +
  geom_abline(slope = 1, intercept = 0, linewidth = border_size) +
  coord_fixed(ratio = 1) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_cor_log_k <- ggplot(data_recovery_log_k, aes(x = as.factor(prior_sd), y = correlation, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "r", title = expression("log("*italic(k)*")")) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_cor_s_log_k <- ggplot(data_cor_s_log_k, aes(x = as.factor(prior_sd), y = correlation, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "r", title = expression("s"["log("*italic(k)*")"]), color = "Population SD") +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.7,0.45),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.5, "lines"),
    legend.title = element_text(size = 5, margin = margin(b = 0)),
    legend.text = element_text(size = 5),
    legend.key.height = unit(0.2, "cm"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_diff_s_log_k <- ggplot(data_diff_s_log_k, aes(x = as.factor(prior_sd), y = diff_s_log_k, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", y = "Mean absolute difference", title = expression("s"["log("*italic(k)*")"])) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

multiplot_recovery <- (scatter_log_k | scatter_s_log_k) / (plot_cor_log_k | plot_cor_s_log_k) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave(file.path("plots", "multiplot_recovery.pdf"), plot = multiplot_recovery, width = 6, height = 6, units = "in", dpi = 300)


multiplot_s_log_k <- (scatter_s_log_k | plot_cor_s_log_k | plot_diff_s_log_k) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave(file.path("plots", "recovery_s_log_k.png"), plot = multiplot_s_log_k, width = 6, height = 3, units = "in", dpi = 300)


# Prior sensitivity example
fit_1 <- readRDS(file.path("out", "sd_0_51", "sample_51", "model_prior_sd_0_1.rds"))
fit_2 <- readRDS(file.path("out", "sd_0_2", "sample_2", "model_prior_sd_0_2.rds"))

posterior_1 <- data.frame(mu_s_log_k = extract(fit_1)$mu_s_log_k)
posterior_2 <- data.frame(mu_s_log_k = extract(fit_2)$mu_s_log_k)

y_cord_1 <- approx(density(posterior_1$mu_s_log_k)$x, density(posterior_1$mu_s_log_k)$y, xout = 0)$y
y_cord_2 <- approx(density(posterior_2$mu_s_log_k)$x, density(posterior_2$mu_s_log_k)$y, xout = 0)$y

axis_text_size <- 8
axis_title_size <- 11
border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3
line_width <- 1
point_size <- 1.5
plot_title_size <- 10
point_size <-2

ps_example_1 <- ggplot(posterior_1, aes(x = mu_s_log_k)) +
  labs(x = expression(theta), y = "Density", title = expression("Prior " * italic("SD") *  "= 0.1")) +
  geom_density(aes(linetype = "Posterior"), key_glyph = draw_key_path) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 0.1), aes(linetype = "Prior")) +
  annotate("point", x = 0, y = y_cord_1, size = point_size) + 
  annotate("point", x = 0, y = dnorm(0,0,0.1), size = point_size) + 
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.2,0.85),
    legend.background = element_rect('transparent'),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.height = unit(0.2, "cm"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

ps_example_2 <- ggplot(posterior_2, aes(x = mu_s_log_k)) +
  labs(x = expression(theta), title = expression("Prior " * italic("SD") *  "= 0.2")) +
  geom_density(aes(linetype = "Posterior")) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 0.2), aes(linetype = "Prior")) +
  annotate("point", x = 0, y = y_cord_2, size = point_size) + 
  annotate("point", x = 0, y = dnorm(0,0,0.2), size = point_size) + 
  theme(
  panel.background = element_blank(),
  panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
  axis.ticks = element_line(linewidth = tick_width),
  axis.ticks.length = unit(tick_length, 'cm'),
  legend.position = 'none',
  axis.title.x = element_text(size = axis_title_size),
  axis.text.x = element_text(size = axis_text_size, color = "black"),
  axis.title.y = element_blank(),
  axis.text.y = element_text(size = axis_text_size, color = "black"),
  plot.title = element_text(hjust = 0.5, size = plot_title_size)
)

ps_example <- (ps_example_1 | ps_example_2) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 12))

ggsave(file.path("plots", "prior_sensitivity_example.pdf"), plot = ps_example, width = 6, height = 3, units = "in", dpi = 300)


# Partial pooling example
fit <- readRDS(file.path("out", "sd_0_2", "sample_1", "model_prior_sd_0_05.rds"))
mu_s_log_k <- summary(fit)$summary[["mu_s_log_k","mean"]]

data_shrinkage_example <- df_recovery[c(1:40), c("true_s_log_k", "median_s_log_k")]
data_shrinkage_example$id <- seq(1:40)
data_shrinkage_example <- pivot_longer(data_shrinkage_example, names_to = "type", values_to = "value", 
                                       cols = c("true_s_log_k", "median_s_log_k"))
data_shrinkage_example$type <- factor(data_shrinkage_example$type, 
                                      levels = c("true_s_log_k", "median_s_log_k"))

border_size <- 0.3
tick_length <- -0.05
tick_width <- 0.3

plot_shrinkage <- ggplot(data_shrinkage_example, aes(x = type, y = value, group = id)) +
  labs(y = expression(theta)) +
  geom_point(size = 1) +
  geom_line(linewidth = 0.2) +
  geom_hline(yintercept = mu_s_log_k, linewidth = 0.3, linetype = "dashed") +
  scale_x_discrete(labels = c("True values", "Estimates")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size = 7, color = "black")
  )

ggsave(file.path("plots", "shrinkage_example.pdf"), plot = plot_shrinkage, width = 3, height = 3, units = "in", dpi = 300)




