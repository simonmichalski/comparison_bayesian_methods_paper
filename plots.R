library("bayesplot")
library("ggplot2")
library("patchwork")
library("tidyr")
library("dplyr")
library("rstan")
library("scales")#

if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

df_results <- readRDS(file.path("results", "results.rds"))
df_sim_thres <- readRDS(file.path("results", "sim_thres.rds"))
df_recovery <- readRDS(file.path("results", "recovery.rds"))
df_sim_thres_effect_prior <- readRDS(file.path("results", "sim_thres_per_effect_and_prior_sd.rds"))

# Values
df_results$s_log_k_sd <- format(df_results$s_log_k_sd, nsmall = 0.01)

dodge_width <- 0.9
boxplot_size <- 0.225
outlier_size = 0.3
axis_text_size <- 7
axis_title_size <- 8
border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2

plot_savage_dickey_bf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = savage_dickey_bf, color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior " * italic("SD")), y = expression("Savage-Dickey BF"[10]), color = expression("Population " * italic("SD"))) +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(labels = c("0.05", "0.10", "0.20", "0.50", "1.00", "1.50", "2.00", "2.50")) +
  geom_boxplot(
    position = position_dodge(width = dodge_width),
    size = boxplot_size,
    outlier.size = outlier_size,
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks.length = unit(tick_length, 'cm'),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    legend.position = c(0.2,0.13),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.4, "lines"),
    legend.title = element_text(size = 5, margin = margin(b = 0)),
    legend.text = element_text(size = 5),
    legend.key.height = unit(0.1, "cm"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black", angle = 30, vjust = 1.2, hjust = 1)
  )

plot_dbf <- ggplot(df_results, aes(x = as.factor(prior_sd), y = directional_bf, color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior " * italic("SD")), y = expression("dBF"["+-"])) +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(labels = c("0.05", "0.10", "0.20", "0.50", "1.00", "1.50", "2.00", "2.50")) +
  geom_boxplot(
    position = position_dodge(width = dodge_width),
    size = boxplot_size,
    outlier.size = outlier_size,
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks.length = unit(tick_length, 'cm'),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black", angle = 30, vjust = 1.2, hjust = 1)
  )


plot_p_effect <- ggplot(df_results, aes(x = as.factor(prior_sd), y = p_effect, color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior " * italic("SD")), y = "P(effect > 0)") +
  scale_x_discrete(labels = c("0.05", "0.10", "0.20", "0.50", "1.00", "1.50", "2.00", "2.50")) +
  geom_boxplot(
    position = position_dodge(width = dodge_width),
    size = boxplot_size,
    outlier.size = outlier_size,
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks.length = unit(tick_length, 'cm'),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black", angle = 30, vjust = 1.2, hjust = 1)
  )


multiplot_values <- plot_savage_dickey_bf + plot_dbf + plot_p_effect +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave("plots/values.pdf", plot = multiplot_values, width = 6, height = 2, units = "in", dpi = 300)


# False positive results (conv)
data_fp_savage_dickey_bf_3 <- aggregate(fp_savage_dickey_bf_3 ~ prior_sd + s_log_k_sd, df_results, sum)

data_fp_directional_bf_3 <- aggregate(fp_directional_bf_3_pos + fp_directional_bf_1_3_neg ~ prior_sd + s_log_k_sd, df_results, sum)
names(data_fp_directional_bf_3)[names(data_fp_directional_bf_3) == "fp_directional_bf_3_pos + fp_directional_bf_1_3_neg"] <- "fp_directional_bf_3"
data_fp_directional_bf_3$s_log_k_sd <- format(data_fp_directional_bf_3$s_log_k_sd, nsmall = 0.01)

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


axis_text_size <- 7
axis_title_size <- 8
plot_title_size <- 7
border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2
line_width <- 1
hline_width <- border_size/2

plot_fp_savage_dickey_bf_3 <- ggplot(data_fp_savage_dickey_bf_3, aes(x = prior_sd, y = (fp_savage_dickey_bf_3/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(y = "% false positives", title = expression("Savage-Dickey" ~ BF[10] > 3)) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_95 <- ggplot(data_fp_p_effect_95, aes(x = prior_sd, y = (fp_p_effect_95/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.05, .95]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_975 <- ggplot(data_fp_p_effect_975, aes(x = prior_sd, y = (fp_p_effect_975/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.025, .975]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_p_effect_99 <- ggplot(data_fp_p_effect_99, aes(x = prior_sd, y = (fp_p_effect_99/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = "Prior SD", title = "P(effect > 0) [.01, .99]") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_3 <- ggplot(data_fp_directional_bf_3, aes(x = prior_sd, y = (fp_directional_bf_3/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(y = "% false positives", title = expression("dBF"["+-"] ~~ "[1/3, 3]"), color = expression("Population "*italic("SD"))) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.65,0.4),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.5, "lines"),
    legend.title = element_text(size = 7, margin = margin(b = 0)),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.2, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_10 <- ggplot(data_fp_directional_bf_10, aes(x = prior_sd, y = (fp_directional_bf_10/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(title = expression("dBF"["+-"] ~~ "[1/10, 10]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_30 <- ggplot(data_fp_directional_bf_30, aes(x = prior_sd, y = (fp_directional_bf_30/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(title = expression("dBF"["+-"] ~~ "[1/30, 30]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_dbf_100 <- ggplot(data_fp_directional_bf_100, aes(x = prior_sd, y = (fp_directional_bf_100/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(title = expression("dBF"["+-"] ~~ "[1/100, 100]")) +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_80 <- ggplot(data_fp_hdi_80, aes(x = prior_sd, y = (fp_hdi_80/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), y = "% false positives", title = "80% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_90 <- ggplot(data_fp_hdi_90, aes(x = prior_sd, y = (fp_hdi_90/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), title = "90% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_95 <- ggplot(data_fp_hdi_95, aes(x = prior_sd, y = (fp_hdi_95/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), title = "95% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  )

plot_fp_hdi_99 <- ggplot(data_fp_hdi_99, aes(x = prior_sd, y = (fp_hdi_99/200)*100, group = as.factor(s_log_k_sd), color = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), title = "99% HDI") +
  geom_line(linewidth = line_width) +
  geom_hline(yintercept = 5, linetype = 'dashed', linewidth = hline_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
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
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 8))

ggsave("plots/false_positives.pdf", plot = multiplot_fp, width = 6, height = 4, units = "in", dpi = 300)


# Adjusted simulation-based decision thresholds (asbdt)
axis_text_size <- 7
axis_title_size <- 7
border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2
line_width <- 1

plot_asbdt_savage_dickey_bf <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = savage_dickey_bf, group = 1)) +
  labs(y = expression("Savage-Dickey BF"[10])) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_asbdt_directional_bf_lower <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = directional_bf_lower, group = 1)) +
  labs(y = expression("dBF"["+-"] ~~ "lower")) +
  geom_line(linewidth = line_width) +
  scale_y_continuous(breaks = c(0.01, 0.02, 0.0333333333), 
                     labels = c("1/100", "1/50", "1/30")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_asbdt_directional_bf_upper <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = directional_bf_upper, group = 1)) +
  labs(y = expression("dBF"["+-"] ~~ "upper")) +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
  
plot_asbdt_hdi_bf <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = hdi*100, group = 1)) +
  labs(x = expression(italic("n")*" tests"), y = "HDI width (%)") +
  geom_line(linewidth = line_width) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_asbdt_p_effect_bf_lower <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = p_effect_lower, group = 1)) +
  labs(x = expression(italic("n")*" tests"), y = "P(effect > 0) lower") +
  geom_line(linewidth = line_width) +
  scale_y_continuous(breaks = c(0.01, 0.02, 0.03), 
                     labels = c(".01", ".02", ".03")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_asbdt_p_effect_bf_upper <- ggplot(df_sim_thres, aes(x = as.factor(n_tests), y = p_effect_upper, group = 1)) +
  labs(x = expression(italic("n")*" tests"), y = "P(effect > 0) upper") +
  geom_line(linewidth = line_width) +
  scale_y_continuous(breaks = c(0.96, 0.97, 0.98, 0.99, 1), 
                     labels = c(".96", ".97", ".98", ".99", "1.00")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )


multiplot_asbd <- (plot_asbdt_savage_dickey_bf | plot_asbdt_directional_bf_lower | plot_asbdt_directional_bf_upper) / 
  (plot_asbdt_hdi_bf | plot_asbdt_p_effect_bf_lower | plot_asbdt_p_effect_bf_upper) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10))

ggsave("plots/thresholds_multiple_tests.pdf", plot = multiplot_asbd, width = 6, height = 2.66, units = "in", dpi = 300)


# Parameter recovery
sd_true_s_log_k <- aggregate(df_recovery, true_s_log_k ~ prior_sd + s_log_k_sd + sample, sd)
sd_estimates_s_log_k <- aggregate(df_recovery, median_s_log_k ~ prior_sd + s_log_k_sd + sample, sd)
data_sd_subj <- merge(sd_true_s_log_k, sd_estimates_s_log_k, by = c("prior_sd", "s_log_k_sd", "sample"))
data_sd_subj$s_log_k_sd <- format(data_sd_subj$s_log_k_sd, nsmall = 0.01)

true_sample_means_s_log_k <- aggregate(df_recovery, true_s_log_k ~ s_log_k_sd + prior_sd + sample, mean)
true_abs_mean_sample_means_s_log_k <- aggregate(true_sample_means_s_log_k, abs(true_s_log_k) ~ s_log_k_sd, mean)

data_group_level_s_log_k <- aggregate(df_results, abs(median_mu_s_log_k) ~ prior_sd + s_log_k_sd, mean)
data_group_level_s_log_k$s_log_k_sd <- format(data_group_level_s_log_k$s_log_k_sd, nsmall = 0.01)


axis_text_size <- 8
axis_title_size <- 10
border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2
line_width <- 1
point_size <- 1.5
plot_title_size <- 11
tick_margin <- 4


scatter_sd_s_log_k <- ggplot(data_sd_subj, 
                             aes(x = true_s_log_k, y = median_s_log_k, color = as.factor(s_log_k_sd), 
                              group = as.factor(s_log_k_sd))) +
  labs(x = expression(italic("SD")*" true parameters"), y = expression(italic("SD")*" estimates"), 
       title = expression("s"["log("*italic(k)*")"]), color = expression("Population "*italic("SD"))) +
  geom_jitter(
    size = 0.1
  ) +
  scale_x_continuous(limits = c(0,1.1), breaks = c(0,0.2,0.4,0.6,0.8,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1), breaks = c(0,0.2,0.4,0.6,0.8,1), expand = c(0, 0)) +
  geom_abline(slope = 1, intercept = 0, linewidth = border_size/2) +
  coord_fixed(ratio = 1, clip = "off") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size/2),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.3,0.8),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.5, "points"),
    legend.title = element_text(size = 8, margin = margin(b = 0)),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.3, "cm"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black", margin = margin(r = tick_margin)),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black", margin = margin(t = tick_margin)),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 1))
  )

plot_group_level_s_log_k <- ggplot(data_group_level_s_log_k, aes(x = prior_sd, y = `abs(median_mu_s_log_k)`, color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) + 
  geom_segment(aes(y = true_abs_mean_sample_means_s_log_k[1,2], x = 0.05, xend = 2.5, linetype = "True"), color = "#F8766D", linewidth = 0.3) +
  geom_segment(aes(y = true_abs_mean_sample_means_s_log_k[2,2], x = 0.05, xend = 2.5, linetype = "True"), color = "#00BA38", linewidth = 0.3) +
  geom_segment(aes(y = true_abs_mean_sample_means_s_log_k[3,2], x = 0.05, xend = 2.5, linetype = "True"), color = "#619CFF", linewidth = 0.3) +
  geom_line(aes(linetype = "Estimate"), linewidth = 1) +
  labs(x = expression("Prior " * italic("SD")), y = "Mean abs. group-level mean", 
       title = expression("s"["log("*italic(k)*")"])) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "0.10", "0.20", "0.50", "1.00", "", "", "2.50")) +
  scale_y_continuous(breaks = c(0.03, 0.06, 0.09, 0.12)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.05,0.75),
    legend.justification = c(0,0.5),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(1, "lines"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.3, "cm"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = plot_title_size)
  ) +
  scale_linetype_manual(values = c("solid","dashed")) +
  guides(color = "none", linetype = guide_legend(override.aes = list(color = "black")))

multiplot_recovery <- (scatter_sd_s_log_k | plot_group_level_s_log_k) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 12))

ggsave(file.path("plots", "recovery_s_log_k.pdf"), plot = multiplot_recovery, width = 6, height = 3, units = "in", dpi = 300)


# Prior sensitivity example
fit_1 <- readRDS(file.path("out", "sd_0_2", "sample_167", "model_prior_sd_0_1.rds"))
fit_2 <- readRDS(file.path("out", "sd_0_2", "sample_167", "model_prior_sd_1_5.rds"))

posterior_1 <- data.frame(mu_s_log_k = extract(fit_1)$mu_s_log_k)
posterior_2 <- data.frame(mu_s_log_k = extract(fit_2)$mu_s_log_k)

y_cord_1 <- approx(density(posterior_1$mu_s_log_k)$x, density(posterior_1$mu_s_log_k)$y, xout = 0)$y
y_cord_2 <- approx(density(posterior_2$mu_s_log_k)$x, density(posterior_2$mu_s_log_k)$y, xout = 0)$y

axis_text_size <- 8
axis_title_size <- 11
border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2
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
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
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
  labs(x = expression(theta), title = expression("Prior " * italic("SD") *  "= 1.5")) +
  geom_density(aes(linetype = "Posterior")) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1.5), aes(linetype = "Prior")) +
  annotate("point", x = 0, y = y_cord_2, size = point_size) + 
  annotate("point", x = 0, y = dnorm(0,0,1.5), size = point_size) + 
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(
  panel.background = element_blank(),
  panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
  axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
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
mu_s_log_k <- median(extract(fit)$mu_s_log_k)

data_shrinkage_example <- df_recovery[c(1:40), c("true_s_log_k", "median_s_log_k")]
data_shrinkage_example$id <- seq(1:40)
data_shrinkage_example <- pivot_longer(data_shrinkage_example, names_to = "type", values_to = "value", 
                                       cols = c("true_s_log_k", "median_s_log_k"))
data_shrinkage_example$type <- factor(data_shrinkage_example$type, 
                                      levels = c("true_s_log_k", "median_s_log_k"))

border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2

plot_shrinkage <- ggplot(data_shrinkage_example, aes(x = type, y = value, group = id)) +
  labs(y = expression(theta["i"])) +
  geom_point(size = 1) +
  geom_line(linewidth = 0.2) +
  geom_hline(yintercept = mu_s_log_k, linewidth = 0.3, linetype = "dashed") +
  scale_x_discrete(labels = c("True values", "Estimates")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size = 7, color = "black")
  )

ggsave(file.path("plots", "shrinkage_example.pdf"), plot = plot_shrinkage, width = 3, height = 3, units = "in", dpi = 300)


# Simulation-based decision thresholds (sbdt)
df_sim_thres_effect_prior$s_log_k_sd <- format(df_sim_thres_effect_prior$s_log_k_sd, nsmall = 0.01)

axis_text_size <- 7
axis_title_size <- 8
border_size <- 0.3
tick_length <- -0.05
tick_width <- border_size/2
line_width <- 1

plot_sbdt_savage_dickey_bf <- ggplot(df_sim_thres_effect_prior, 
                                     aes(x = prior_sd, y = savage_dickey_bf, 
                                         color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) +
  labs(y = expression("Savage-Dickey BF"[10])) +
  geom_line(linewidth = line_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_sbdt_directional_bf_lower <- ggplot(df_sim_thres_effect_prior, 
                                         aes(x = prior_sd, y = directional_bf_lower, 
                                             color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) +
  labs(y = expression("dBF"["+-"] ~~ "lower"), color = expression("Population "*italic("SD"))) +
  geom_line(linewidth = line_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  scale_y_continuous(breaks = c(0.0333333333, 0.1, 0.2), 
                     labels = c("1/30", "1/10", "1/5")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = c(0.7,0.65),
    legend.background = element_rect('transparent'),
    legend.key.size = unit(0.5, "lines"),
    legend.title = element_text(size = 7, margin = margin(b = 0)),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.2, "cm"),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_sbdt_directional_bf_upper <- ggplot(df_sim_thres_effect_prior, 
                                         aes(x = prior_sd, y = directional_bf_upper, 
                                             color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) +
  labs(y = expression("dBF"["+-"] ~~ "upper")) +
  geom_line(linewidth = line_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot_sbdt_hdi_bf <- ggplot(df_sim_thres_effect_prior, 
                           aes(x = prior_sd, y = hdi*100, 
                               color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), y = "HDI width (%)") +
  geom_line(linewidth = line_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_sbdt_p_effect_bf_lower <- ggplot(df_sim_thres_effect_prior,
                                      aes(x = prior_sd, y = p_effect_lower,
                                          color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), y = "P(effect > 0) lower") +
  geom_line(linewidth = line_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  scale_y_continuous(breaks = c(0.05, 0.1, 0.15, 0.2), 
                     labels = c(".05", ".10", ".15", ".20")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

plot_sbdt_p_effect_bf_upper <- ggplot(df_sim_thres_effect_prior,
                                      aes(x = prior_sd, y = p_effect_upper, 
                                          color = as.factor(s_log_k_sd), group = as.factor(s_log_k_sd))) +
  labs(x = expression("Prior "*italic("SD")), y = "P(effect > 0) upper") +
  geom_line(linewidth = line_width) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5),
                labels = c("0.05", "", "0.20", "", "1.00", "", "", "2.50")) +
  scale_y_continuous(breaks = c(0.8, 0.9), 
                     labels = c(".80", ".90")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = border_size),
    axis.ticks = element_line(linewidth = tick_width, color = "#000000"),
    axis.ticks.length = unit(tick_length, 'cm'),
    legend.position = 'none',
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_title_size),
    axis.text.x = element_text(size = axis_text_size, color = "black")
  )

multiplot_sim_thres <- (plot_sbdt_savage_dickey_bf | plot_sbdt_directional_bf_lower | plot_sbdt_directional_bf_upper) / 
  (plot_sbdt_hdi_bf | plot_sbdt_p_effect_bf_lower | plot_sbdt_p_effect_bf_upper) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 8))

ggsave("plots/sim_thres.pdf", plot = multiplot_sim_thres, width = 6, height = 2.66, units = "in", dpi = 300)
